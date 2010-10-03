/** \file firmware/main.c
 * \brief The firmware for ATmega devices
 *
 * \author Copyright (C) 2009 samplemaker
 * \author Copyright (C) 2010 samplemaker
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301 USA
 *
 * \defgroup firmware Firmware
 *
 * \defgroup firmware_memories Memory types and layout
 * \ingroup firmware
 *
 * There can be a number of kinds of variables.
 *
 *   a) Uninitialized non-register variables.  Those are placed in the
 *      .bss section in the ELF file, and in the SRAM on the device.
 *      The whole SRAM portion corresponding to the .bss section is
 *      initialized to zero bytes in the startup code, so we do not
 *      need to initialized those variables anywhere.
 *
 *   b) Initialized non-register variables.  Those are placed in the
 *      .data section in the ELF file, and in the SRAM on the device.
 *      The whole SRAM portion corresponding to the .data section is
 *      initialized to their respective byte values by autogenerated
 *      code executed before main() is run.
 *
 *   c) Initialized constants in the .text section.  Those are placed
 *      into the program flash on the device, and due to the AVR's
 *      Harvard architecture, need special instructions to
 *      read. Unused so far.
 *
 *   d) Register variables.  We only use them in the uninitialized
 *      variety so far for the assembly language version
 *      ISR(ADC_vect), if you choose to compile and link that.
 *
 *   e) EEPROM variables.  We are not using those anywhere yet.
 *
 * All in all, this means that for normal memory variables,
 * initialized or uninitialized, we do not need to initialize anything
 * at the start of main().
 *
 * Also note that the ATmega644 has 4K of SRAM. With an ADC resolution
 * of 10 bit, we need to store 2^10 = 1024 = 1K values in our
 * histogram table. This results in the following memory sizes for the
 * histogram table:
 *
 *    uint16_t: 2K
 *    uint24_t: 3K
 *    uint32_t: 4K
 *
 * We could fit the global variables into otherwise unused registers
 * to free some more SRAM, but we cannot move the stack into register
 * space. This means we cannot use uint32_t counters in the table -
 * the absolute maximum sized integer we can use is our self-defined
 * "uint24_t" type.
 *
 * \addtogroup firmware
 * @{
 */

/*------------------------------------------------------------------------------
 * Includes
 *------------------------------------------------------------------------------
 */

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <avr/wdt.h>

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "global.h"
#include "uart-comm.h"
#include "frame-comm.h"
#include "packet-comm.h"
#include "frame-defs.h"
#include "packet-defs.h"
#include "firmware-version.h"


/* Only try compiling for supported MCU types */
#if defined(__AVR_ATmega644__) || defined(__AVR_ATmega644P__)
#else
# error Unsupported MCU!
#endif


/** Define AVR device fuses.
 *
 * CAUTION: These values are highly device dependent.
 *
 * We avoid C99 initializers to make sure that we do initialize each
 * and every fuse value in the structure. */
FUSES = {
  /* 0xd7 = low */ (FUSE_SUT1 & FUSE_CKSEL3),
  /* 0x99 = high */ (FUSE_JTAGEN & FUSE_SPIEN & FUSE_BOOTSZ1 & FUSE_BOOTSZ0),
  /* 0xfc = extended */ (FUSE_BODLEVEL1 & FUSE_BODLEVEL0)
};


/*------------------------------------------------------------------------------
 * Defines
 *------------------------------------------------------------------------------
 */


/** Number of elements in the histogram table */
#define MAX_COUNTER (1<<ADC_RESOLUTION)


/** Trigger AVR reset via watchdog device. */
#define soft_reset()                                            \
  do {                                                          \
    wdt_enable(WDTO_15MS);                                      \
    while (1) {                                                 \
      /* wait until watchdog has caused a system reset */       \
    }                                                           \
  } while(0)


/*------------------------------------------------------------------------------
 * Variables  (static, not visible in other modules)
 *------------------------------------------------------------------------------
 */


/** \var table
 * \brief histogram table
 *
 * ATmega644P has 4Kbyte RAM.  When using 10bit ADC resolution,
 * MAX_COUNTER==1024 and 24bit values will still fit (3K table).
 **/
volatile histogram_element_t table[MAX_COUNTER];


/** timer counter
 *
 * Initialized once by main() with value received from host
 * controller. Never touched by main() again after starting the timer
 * interrupt.
 *
 * Timer interrupt handler has exclusive access to read/writes
 * timer_count to decrement, once the timer ISR has been enabled.
 */
volatile uint16_t timer_count;


/** timer multiple
  *
  * Is send by hostware. Number of dropped analog samples (downsampling of
  * analog signal sampled with timer1 time base)
  */
volatile uint16_t timer_multiple;


/** Last value of timer counter
 *
 * Used for pseudo synchronized reading of the timer_count multi-byte
 * variable in the main program, while timer_count may be written to
 * by the timer ISR.
 *
 * \see get_duration, ISR(TIMER1_COMPA_vect)
 */
volatile uint16_t last_timer_count = 1;


/** Original timer count received in the command.
 *
 * Used later for determining how much time has elapsed yet. Written
 * once only, when the command has been received.
 */
volatile uint16_t orig_timer_count;


/** Timer counter has reached zero.
 *
 * Used to signal from the timer ISR to the main program that the
 * timer has elapsed.
 *
 * Will be set to 1 when max_timer_count is exceeded, is 0 otherwise.
 * Written only once by timer interrupt handler. Read by main
 * loop. 8bit value, and thus accessible with atomic read/write
 * operations.
 */
volatile uint8_t timer_flag;


/*------------------------------------------------------------------------------
 * Local prototypes (not visible in other modules)
 *------------------------------------------------------------------------------
 */


/** Disable watchdog on device reset.
 *
 * Newer AVRs do not disable the watchdog on reset, so we need to
 * disable it manually early in the startup sequence. "Newer" AVRs
 * include the 164P/324P/644P we are using.
 *
 * See http://www.nongnu.org/avr-libc/user-manual/FAQ.html#faq_softreset
 */
void wdt_init(void) __attribute__((naked)) __attribute__((section(".init3")));
void wdt_init(void)
{
  MCUSR = 0;
  wdt_disable();
  return;
}

/** AD conversion complete interrupt entry point
  *
  * This function is called when an A/D conversion has completed.
  * Downsampling of base analog samples and update of histogram table.
  * Actually one could implement a low pass filter here before
  * downsampling to fullfill shannons sample theoreme
  */
ISR(ADC_vect)
{
  /* downsampling of analog data as a multiple of timer_multiple      */
  if (orig_timer_count == timer_multiple){
      /* Read analog value */
      uint16_t result = ADCW;
      /* cut off 2, 1 or 0 LSB */
      const uint16_t index = result >> (10-ADC_RESOLUTION);
      /* For 24bit values, the source code looks a little more complicated
       * than just table[index]++ (even though the generated machine
       * instructions are not).  Anyway, we needed to move the increment
       * into a properly defined _inc function.
       */
       volatile histogram_element_t *element = &(table[index]);
       histogram_element_inc(element);
       timer_multiple = 0;
  } else {
       timer_multiple++;
  }

  /** \todo really necessary? */
  /* Clear interrupt flag of timer1 compare match A & B manually since there is no
     TIMER1_COMPB_vect ISR executed                                      */
  TIFR1 |= _BV(OCF1B);
  //TIFR1 |= _BV(OCF1A);
}


/* runs open end */
/*
ISR(TIMER1_COMPA_vect)
{
}
*/


/* just return the sample rate */
inline static
uint16_t get_duration(void)
{
  return orig_timer_count;
}


/** ADC initialisation and configuration
 *
 * ADC configured as auto trigger
 * Trigger source compare register B
 * Use external analog reference AREF at PIN 32
 * AD input channel on Pin 40 ADC0
 */
inline static
void adc_init(void)
{
  uint16_t result;

  /* channel number: PIN 40 ADC0 -> ADMUX=0 */
  ADMUX = 0;

  /* select voltage reference: external AREF Pin 32 as reference */
  ADMUX &= ~(_BV(REFS1) | _BV(REFS0));

  /* clear ADC Control and Status Register A
   * enable ADC & configure IO-Pins to ADC (ADC ENable) */
  ADCSRA = _BV(ADEN);

  /* ADC prescaler selection (ADC Prescaler Select Bits) */
  /* bits ADPS0 .. ADPS2 */
  ADCSRA |= ((((ADC_PRESCALER >> 2) & 0x1)*_BV(ADPS2)) |
             (((ADC_PRESCALER >> 1) & 0x1)*_BV(ADPS1)) |
              ((ADC_PRESCALER & 0x01)*_BV(ADPS0)));

  /* dummy read out (first conversion takes some time) */
  /* software triggered AD-Conversion */
  ADCSRA |= _BV(ADSC);

  /* wait until conversion is complete */
  loop_until_bit_is_clear(ADCSRA, ADSC);

  /* clear returned AD value, other next conversion value is not ovrtaken */
  result = ADCW;

  /* Enable AD conversion complete interrupt if I-Flag in sreg is set
   * (-> ADC interrupt enable) */
  ADCSRA |= _BV(ADIE);

  /* Configure ADC trigger source:
   * Select external trigger trigger ADC on Compare Match B of Timer1 */
  ADCSRB = (_BV(ADTS2)|_BV(ADTS0));

  /* ADC auto trigger enable: ADC will be started by trigger signal */
  ADCSRA |= _BV(ADATE);
}


/** Configure 16 bit timer to trigger an ISR every 0.1 second
 *
 * Configure "measurement in progress toggle LED-signal"
 */
inline static
void timer_init(const uint8_t timer0, const uint8_t timer1)
{
  /** Set up timer with the combined value we just got the bytes of.
   *
   * For some reasons, the following line triggers a bug with
   * the avr-gcc 4.4.2 and 4.5.0 we have available on Fedora
   * 12 and Fedora 13. Debian Lenny (5.05)'s avr-gcc 4.3.2
   * does not exhibit the buggy behaviour, BTW. So we do the
   * assignments manually here.
   *
   * orig_timer_count = (((uint16_t)timer1)<<8) | timer0;
   * timer_count = orig_timer_count;
   */
  asm("\n\t"
      "sts orig_timer_count,   %[timer0]\n\t"
      "sts orig_timer_count+1, %[timer1]\n\t"
      "sts timer_count,   %[timer0]\n\t"
      "sts timer_count+1, %[timer1]\n\t"
      : /* output operands */
      : /* input operands */
        [timer0] "r" (timer0),
        [timer1] "r" (timer1)
      );

  /* Prepare timer 0 control register A and B for
     clear timer on compare match (CTC)                           */
  TCCR1A = 0;
  TCCR1B =  _BV(WGM12);

  /* Configure "measurement in progress LED"                      */
  /* configure pin 19 as an output */
  DDRD |= (_BV(DDD5));
  /* toggle LED pin 19 on compare match automatically             */
  TCCR1A |= _BV(COM1A0);

  /* toggle pin on port PD4 in case of a compare match B  */
  DDRD |= (_BV(DDD4));
  TCCR1A |= _BV(COM1B0);

  /* Prescaler settings on timer conrtrol reg. B                  */
  TCCR1B |=  ((((TIMER_PRESCALER >> 2) & 0x1)*_BV(CS12)) |
              (((TIMER_PRESCALER >> 1) & 0x1)*_BV(CS11)) |
              ((TIMER_PRESCALER & 0x01)*_BV(CS10)));

  /* Derive sample rate (time base) as a multiple of the base
     compare match value for 0.1sec. Write to output compare
     reg. A                                                       */
  OCR1A = (TIMER_COMPARE_MATCH_VAL);

  /* The ADC can only be triggered via compare register B.
     Set the trigger point (compare match B) to 50% of
     compare match A                                              */
  OCR1B = (TIMER_COMPARE_MATCH_VAL >> 1);

  /* we do not need to jump to any ISRs since we do everything
     inside the ADC callback function                             */

  /* output compare match B interrupt enable                      */
  //TIMSK1 |= BIT(OCIE1B);

  /* output compare match A interrupt enable                      */
  //TIMSK1 |= _BV(OCIE1A);
}


/** Initialize peripherals
 *
 * Configure peak hold capacitor reset pin
 * Configure unused pins
 */
inline static
void io_init(void)
{

}


/** \addtogroup firmware_comm
 * @{
 */


#ifdef INVENTED_HISTOGRAM

/** created from binary via objcopy */
extern uint8_t invented_histogram[] asm("_binary_invented_histogram_bin_start") PROGMEM;
/** created from binary via objcopy */
extern uint8_t invented_histogram_size[] asm("_binary_invented_histogram_bin_size") PROGMEM;
/** created from binary via objcopy */
extern uint8_t invented_histogram_end[] asm("_binary_invented_histogram_bin_end") PROGMEM;

#if (ELEMENT_SIZE_IN_BYTES == 3)
/** Simulate a histogram based on the invented histogram data */
static
void invent_histogram(const uint16_t duration)
{
  uint8_t *t8 = (uint8_t *)table;
  for (size_t j=0; j<3*MAX_COUNTER; j+=3) {
    const uint32_t v =
      (((uint32_t)pgm_read_byte(&(invented_histogram[j+0])))<< 0) +
      (((uint32_t)pgm_read_byte(&(invented_histogram[j+1])))<< 8) +
      (((uint32_t)pgm_read_byte(&(invented_histogram[j+2])))<<16);
    const uint32_t r = (v*duration) >> 8;
    t8[j+0] = (r>> 0) & 0xff;
    t8[j+1] = (r>> 8) & 0xff;
    t8[j+2] = (r>>16) & 0xff;
  }
}
#endif

#endif


/** Send histogram packet to controller via serial port (layer 3).
 *
 * \param type The type of histogram we are sending
 *             (#packet_histogram_type_t).  You may also a dummy value
 *             like '?' or -1 or 0xff or 0 until you make use of that
 *             value on the receiver side.
 *
 * Note that send_histogram() might take a significant amount of time.
 * For example, at 9600bps, transmitting a good 3KByte will take a
 * good 3 seconds.  If you disable interrupts for that time and want
 * to continue the measurement later, you will want to properly pause
 * the timer.  We are currently keeping interrupts enabled if we
 * continue measuring, which avoids this issue.
 *
 * Note that for 'I' histograms it is possible that we send fluked
 * values due to overflows.
 */
static
void send_histogram(const packet_histogram_type_t type);
static
void send_histogram(const packet_histogram_type_t type)
{
  const uint16_t duration = get_duration();

#ifdef INVENTED_HISTOGRAM
  invent_histogram(duration);
#endif

  packet_histogram_header_t header = {
    ELEMENT_SIZE_IN_BYTES,
    type,
    duration,
    orig_timer_count
  };
  frame_start(FRAME_TYPE_HISTOGRAM, sizeof(header)+sizeof(table));
  uart_putb((const void *)&header, sizeof(header));
  uart_putb((const void *)table, sizeof(table));
  frame_end();
}


/** @} */


/** List of states for firmware state machine
 *
 * \see communication_protocol
 */
typedef enum {
  ST_READY,
  ST_timer0,
  ST_timer1,
  ST_checksum,
  ST_MEASURING,
  ST_MEASURING_nomsg,
  ST_DONE,
  ST_RESET
} firmware_state_t;


/** AVR firmware's main "loop" function
 *
 * Note that we create a "loop" by having the watchdog timer reset the
 * AVR device when one loop iteration is finished. This will cause the
 * system to start again with the hardware and software in the defined
 * default state.
 *
 * This function implements the finite state machine (FSM) as
 * described in \ref embedded_fsm.  The "ST_foo" and "ST_FOO"
 * definitions from #firmware_state_t refer to the states from that FSM
 * definition.
 *
 * Note that the ST_MEASURING state had to be split into two:
 * ST_MEASURING which prints its name upon entering and immediately
 * continues with ST_MEASURING_nomsg, and ST_MEASURING_nomsg which
 * does not print its name upon entering and is thus feasible for a
 * busy polling loop.
 *
 * avr-gcc knows that int main(void) ending with an endless loop and
 * not returning is normal, so we can avoid the
 *
 *    int main(void) __attribute__((noreturn));
 *
 * declaration and compile without warnings (or an unused return instruction
 * at the end of main).
 */
int main(void)
{
    /** No need to initialize global variables here. See \ref firmware_memories. */

    /* ST_booting */

    /* configure USART0 for 8N1 */
    uart_init();
    send_text("Booting");
    send_version();

    /* initialize peripherals */
    io_init();

    /* configure AREF at pin 32 and single shot auto trigger over int0
     * at pin 40 ADC0 */
    adc_init();

    /** Used while receiving "m" command */
    register uint8_t timer0 = 0;
    /** Used while receiving "m" command */
    register uint8_t timer1 = 0;

    /** Firmware FSM state */
    firmware_state_t state = ST_READY;

    /* Firmware FSM loop */
    while (1) {
      /** Used in several places when reading in characters from UART */
      char ch;
      /** Used in several places when reading in characters from UART */
      frame_cmd_t cmd;
      /** next FSM state */
      firmware_state_t next_state = state;
      switch (state) {
      case ST_READY:
        send_state("READY");
        uart_checksum_reset();
        cmd = ch = uart_getc();
        switch (cmd) {
        case FRAME_CMD_RESET:
          next_state = ST_RESET;
          break;
        case FRAME_CMD_MEASURE:
          next_state = ST_timer0;
          break;
        case FRAME_CMD_STATE:
          next_state = ST_READY;
          break;
        default: /* ignore all other bytes */
          next_state = ST_READY;
          break;
        } /* switch (cmd) */
        break;
      case ST_timer0:
        timer0 = uart_getc();
        next_state = ST_timer1;
        break;
      case ST_timer1:
        timer1 = uart_getc();
        next_state = ST_checksum;
        break;
      case ST_checksum:
        if (uart_checksum_recv()) { /* checksum successful */
          /* begin measurement */
          timer_init(timer0, timer1);
          sei();
          next_state = ST_MEASURING;
        } else { /* checksum fail */
          /** \todo Find a way to report checksum failure without
           *        resorting to sending free text. */
          send_text("checksum fail");
          next_state = ST_RESET;
        }
        break;
      case ST_MEASURING:
        send_state("MEASURING");
        next_state = ST_MEASURING_nomsg;
        break;
      case ST_MEASURING_nomsg:
        if (timer_flag) { /* done */
          cli();
          send_histogram(PACKET_HISTOGRAM_DONE);
          next_state = ST_DONE;
        } else if (bit_is_set(UCSR0A, RXC0)) {
          /* there is a character in the UART input buffer */
          cmd = ch = uart_getc();
          switch (cmd) {
          case FRAME_CMD_ABORT:
            cli();
            send_histogram(PACKET_HISTOGRAM_ABORTED);
            next_state = ST_RESET;
            break;
          case FRAME_CMD_INTERMEDIATE:
            /** The ISR(ADC_vect) will be called when the analog circuit
             * detects an event.  This will cause glitches in the
             * intermediate histogram values as the histogram values are
             * larger than 8 bits.  However, we have decided that for
             * *intermediate* results, those glitches are acceptable.
             *
             * Keeping interrupts enabled has the additional advantage
             * that the measurement continues during send_histogram(),
             * so we need not concern ourselves with pausing the
             * measurement timer or anything similar.
             *
             * If you decide to bracket the send_histogram() call with a
             * cli()/sei() pair, be aware that you need to solve the
             * issue of resetting the peak hold capacitor on resume if
             * an event has been detected by the analog circuit while we
             * had interrupts disabled and thus ISR(ADC_vect) could not
             * reset the peak hold capacitor.
             */
            send_histogram(PACKET_HISTOGRAM_INTERMEDIATE);
            next_state = ST_MEASURING;
            break;
          case FRAME_CMD_STATE:
            next_state = ST_MEASURING;
            break;
          default: /* ignore all other bytes */
            next_state = ST_MEASURING;
            break;
          }
        } else { /* neither timer flag set nor incoming UART data */
          next_state = ST_MEASURING_nomsg;
        }
        break;
      case ST_DONE:
        /* STATE: DONE (wait for RESET command while seinding histograms) */
        send_state("DONE");
        cmd = ch = uart_getc();
        switch (cmd) {
        case FRAME_CMD_STATE:
          next_state = ST_DONE;
          break;
        case FRAME_CMD_RESET:
          next_state = ST_RESET;
          break;
        default:
          send_histogram(PACKET_HISTOGRAM_RESEND);
          next_state = ST_DONE;
          break;
        }
        break;
      case ST_RESET:
        send_state("RESET");
        soft_reset();
        break;
      } /* switch (state) */
      state = next_state;
    } /* while (1) */
}

/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
