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
#include "frame-defs.h"
#include "packet-defs.h"


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


/** Bit mask shortcut */
#define BIT(NO) (1<<(NO))


/** Trigger AVR reset via watchdog device. */
#define soft_reset()						\
  do {								\
    wdt_enable(WDTO_15MS);					\
    while (1) {							\
      /* wait until watchdog has caused a system reset */	\
    }								\
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
volatile uint16_t last_timer_count = 1;
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
 * Update histogram
 * Discharge peak hold capacitor
 */
ISR(ADC_vect) {

  /* pull pin to discharge peak hold capacitor                    */
  /** \todo worst case calculation: runtime & R7010 */
  PORTD |= BIT(PD6);

  /* Read analog value */
  uint16_t result = ADCW;

  /* We are confident that the range of values the ADC gives us
   * is within the specced 10bit range of 0..1023. */

  /* cut off 2, 1 or 0 LSB */
  const uint16_t index = result >> (10-ADC_RESOLUTION);

  /* For 24bit values, the source code looks a little more complicated
   * than just table[index]++ (even though the generated machine
   * instructions are not).  Anyway, we needed to move the increment
   * into a properly defined _inc function.
   */
  volatile histogram_element_t *element = &(table[index]);
  histogram_element_inc(element);

  /* set pin to GND and release peak hold capacitor   */
  PORTD &=~ BIT(PD6);

  /* If a hardware event on int0 pin occurs an interrupt flag in EIFR is set.
   * Since int0 is only configured but not enabled ISR(INT0_vect){} is
   * not executed and therefore this flag is not reset automatically.
   * To reset this flag the bit at position INTF0 must be set.
   */
  EIFR |= BIT(INTF0);
}


/** 16 Bit timer ISR
 *
 * When timer has elapsed, the global #timer_flag (8bit, therefore
 * atomic read/writes) is set.
 */
ISR(TIMER1_COMPA_vect)
{
  /* toggle a sign PORTD ^= BIT(PD5); (done automatically) */

  if (!timer_flag) {
    /* We do not touch the timer_flag ever again after setting it */
    last_timer_count = timer_count;
    timer_count--;
    if (timer_count == 0) {
      /* timer has elapsed, set the flag to signal the main program */
      timer_flag = 1;
    }
  }
}


/** Setup of INT0
 *
 * INT0 via pin 16 is configured but not enabled
 * Trigger on falling edge
 * Enable pull up resistor on Pin 16 (20-50kOhm)
 */
inline static
void trigger_src_conf(void)
{

    /* Configure INT0 pin 16 as input */
    /* Reset Int0 pin 16 bit DDRD in port D Data direction register */
    DDRD &= ~(BIT(DDD2));
    /* Port D data register: Enable pull up on pin 16, 20-50kOhm */
    PORTD |= BIT(PD2);

    /* Disable interrupt "INT0" (clear interrupt enable bit in
     * external interrupt mask register) otherwise an interrupt may
     * occur during level and edge configuration (EICRA)  */
    EIMSK &= ~(BIT(INT0));
    /* Level and edges on the external pin that activates INT0
     * is configured now (interrupt sense control bits in external
     * interrupt control register A). Disable everything.  */
    EICRA &= ~(BIT(ISC01) | BIT(ISC00));
    /* Now enable interrupt on falling edge.
     * [ 10 = interrupt on rising edge
     *   11 = interrupt on falling edge ] */
    EICRA |=  BIT(ISC01);
    /* Clear interrupt flag by writing a locical one to INTFn in the
     * external interrupt flag register.  The flag is set when a
     * interrupt occurs. if the I-flag in the sreg is set and the
     * corresponding flag in the EIFR the program counter jumps to the
     * vector table  */
    EIFR |= BIT(INTF0);
    /* reenable interrupt INT0 (External interrupt mask
     * register). we do not want to jump to the ISR in case of an interrupt
     * so we do not set this bit  */
    // EIMSK |= (BIT(INT0));

}


/** ADC initialisation and configuration
 *
 * ADC configured as auto trigger
 * Trigger source INT0
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
  ADMUX &= ~(BIT(REFS1) | BIT(REFS0));

  /* clear ADC Control and Status Register A
   * enable ADC & configure IO-Pins to ADC (ADC ENable) */
  ADCSRA = BIT(ADEN);

  /* ADC prescaler selection (ADC Prescaler Select Bits) */
  /* bits ADPS0 .. ADPS2 */
  ADCSRA |= ((((ADC_PRESCALER >> 2) & 0x1)*BIT(ADPS2)) |
             (((ADC_PRESCALER >> 1) & 0x1)*BIT(ADPS1)) |
              ((ADC_PRESCALER & 0x01)*BIT(ADPS0)));

  /* dummy read out (first conversion takes some time) */
  /* software triggered AD-Conversion */
  ADCSRA |= BIT(ADSC);

  /* wait until conversion is complete */
  loop_until_bit_is_clear(ADCSRA, ADSC);

  /* clear returned AD value, other next conversion value is not ovrtaken */
  result = ADCW;

  /* Enable AD conversion complete interrupt if I-Flag in sreg is set
   * (-> ADC interrupt enable) */
  ADCSRA |= BIT(ADIE);

   /* Configure ADC trigger source:
    * Select external trigger "interrupt request 0"
    * Interrupt on rising edge                         */
  ADCSRB |= BIT(ADTS1);
  ADCSRB &= ~(BIT(ADTS0) | BIT(ADTS2));

  /* ADC auto trigger enable: ADC will be started by trigger signal */
  ADCSRA |= BIT(ADATE);
}


/** Configure 16 bit timer to trigger an ISR every second         
 *
 * Configure "measurement in progress toggle LED-signal"
 */
inline static
void timer_init(void){

  /* Prepare timer 0 control register A and B for
     clear timer on compare match (CTC)                           */
  TCCR1A = 0;
  TCCR1B =  BIT(WGM12);

  /* Configure "measurement in progress LED"                      */
  /* configure pin 19 as an output */
  DDRD |= (BIT(DDD5));
  /* toggle LED pin 19 on compare match automatically             */
  TCCR1A |= BIT(COM1A0);

  /* Prescaler settings on timer conrtrol reg. B                  */
  TCCR1B |=  ((((TIMER_PRESCALER >> 2) & 0x1)*BIT(CS12)) |
              (((TIMER_PRESCALER >> 1) & 0x1)*BIT(CS11)) |
              ((TIMER_PRESCALER & 0x01)*BIT(CS10)));

  /* Compare match value into output compare reg. A               */
  OCR1A = TIMER_COMPARE_MATCH_VAL;

  /* output compare match A interrupt enable                      */
  TIMSK1 |= BIT(OCIE1A);
}


/** Configure 16bit timer to trigger an ISR four times as fast ast timer_init() does.
 *
 * You MUST have run timer_init() some time before running timer_init_quick().
 */
inline static
void timer_init_quick(void)
{
  const uint8_t old_tccr1b = TCCR1B;
  /* pause the clock */
  TCCR1B &= ~(BIT(CS12) | BIT(CS11) | BIT(CS10));
  /* faster blinking */
  OCR1A = TIMER_COMPARE_MATCH_VAL / 4;
  /* start counting from 0, needs clock to be paused */
  TCNT1 = 0;
  /* unpause the clock */
  TCCR1B = old_tccr1b;
}


/** Initialize peripherals
 *
 * Configure peak hold capacitor reset pin
 * Configure unused pins
 */
inline static
void io_init(void)
{
    /* configure pin 20 as an output                               */
    DDRD |= (BIT(DDD6));
    /* set pin 20 to ground                                        */
    PORTD &= ~BIT(PD6);

    /** \todo configure unused pins */
}


/** \defgroup firmware_comm Packet Communication
 * \ingroup firmware
 *
 * Implement packet part of the communication protocol (Layer 3).
 *
 * As all multi-byte values sent or received are little-endian, we can
 * just send and receive native values on the AVR and forget about
 * endianness altogether.
 *
 * @{
 */


#define INVENTED_HISTOGRAM


#ifdef INVENTED_HISTOGRAM

/** created from binary via objcopy */
extern uint8_t invented_histogram[] asm("_binary_invented_histogram_bin_start") PROGMEM;
extern uint8_t invented_histogram_size[] asm("_binary_invented_histogram_bin_size") PROGMEM;
extern uint8_t invented_histogram_end[] asm("_binary_invented_histogram_bin_end") PROGMEM;

#if (ELEMENT_SIZE_IN_BYTES == 3)
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
 */
static
void send_histogram(const packet_histogram_type_t type)
{
  /* pseudo synchronised reading of multi-byte variable being written
   * to by ISR */
  uint16_t a, b;
  do {
    a = timer_count;
    b = last_timer_count;
  } while ((b-a) != 1);
  /* Now 'a' contains a valid value */
  const uint16_t duration = orig_timer_count - a;

#ifdef INVENTED_HISTOGRAM
  invent_histogram(duration);
#endif

  packet_histogram_header_t header = {
    ELEMENT_SIZE_IN_BYTES,
    type,
    duration
  };
  frame_start(FRAME_TYPE_HISTOGRAM, sizeof(header)+sizeof(table));
  uart_putb((const void *)&header, sizeof(header));
  uart_putb((const void *)table, sizeof(table));
  frame_end();
}


/** Send state message packet to host (layer 3).
 *
 * State messages are constant strings describing the FSM state we are
 * currently in.
 */
static
void send_state(const char *msg)
{
  const size_t len = strlen(msg);
  frame_send(FRAME_TYPE_STATE, msg, len);
}


/** Send text message packet to host (layer 3).
 *
 * If you need to send more than static text, use uprintf().
 */
static
void send_text(const char *msg)
{
  const size_t len = strlen(msg);
  frame_send(FRAME_TYPE_TEXT, msg, len);
}


/** @} */


/** Go into the RESET state, and reset the machine */
void goto_reset(void) __attribute__((noreturn));
void goto_reset(void)
{
  send_state("RESET");
  soft_reset();
}


/** AVR firmware's main "loop" function
 *
 * Note that we create a "loop" by resetting the AVR device when we
 * are finished, which will cause the system to start again with the
 * hardware and software in the defined default state.
 *
 * The #main function implements the state machine as described in
 * \ref embedded_fsm. The states from that state machine are what the
 * "STATE: FOO" comments in #main refer to.
 *
 * avr-gcc knows that int main(void) ending with an endless loop and
 * not returning is normal, so we can avoid the
 *
 *    int main(void) __attribute__((noreturn));
 *
 * declaration and compile without warnings (or a return instruction
 * at the end of main).
 */
int main(void)
{
    /** No need to initialize variables here. See \ref firmware_memories. */

    /* STATE: BOOT */

    /* configure USART0 for 8N1 */
    uart_init();
    send_text("Booting");

    /* initialize peripherals */
    io_init();

    /* configure INT0 pin 16 */
    trigger_src_conf();

    /* configure AREF at pin 32 and single shot auto trigger over int0
     * at pin 40 ADC0 */
    adc_init();

    /* STATE: READY */
    send_state("READY");

    uint8_t quit_flag = 0;
    uint16_t timer_value;
    while (!quit_flag) {
      uart_checksum_reset();
      const char ch = uart_getc();
      const frame_cmd_t cmd = ch;
      switch (cmd) {
      case FRAME_CMD_RESET:
	/* STATE: RESET */
	goto_reset();
	break;
      case FRAME_CMD_MEASURE:
	if (1) {
	  /* STATUS: TIMER0 */
	  const uint8_t byte0 = uart_getc();
	  /* STATUS: TIMER1 */
	  const uint8_t byte1 = uart_getc();
	  timer_value = (((uint16_t)byte1)<<8) | byte0;
	  /* STATUS: CHECKSUM */
	  if (uart_checksum_recv()) { /* checksum successful */
	    /* NEXT_STATE: MEASURING */
	    quit_flag = 1;
	  } else { /* checksum fail */
	    /** \todo Find a way to report checksum failure without
	     *        resorting to sending free text. */
	    send_text("checksum fail");
	    /* STATE: RESET */
	    goto_reset();
	  }
	}
	break;
      case FRAME_CMD_STATE:
	/* remain in current state READY, just print it */
	break;
      default:
	/* ignore all other bytes */
	/* NEXT_STATE: READY */
	break;
      }
      if (quit_flag)
	break;
      send_state("READY");
    }

    /* STATE: MEASURING */
    send_state("MEASURING");

    /* set up timer with the value we just got */
    timer_count = orig_timer_count = timer_value;

    /* begin measurement */
    timer_init();
    sei();

    while (1) {
      if (timer_flag) { /* done */
	cli();
	send_histogram(PACKET_HISTOGRAM_DONE);
	timer_init_quick();
	while (1) {
	  /* STATE: DONE (wait for RESET command while seinding histograms) */
	  send_state("DONE");
	  const char ch = uart_getc();
	  const frame_cmd_t cmd = ch;
	  switch (cmd) {
	  case FRAME_CMD_STATE:
	    /* remain in current state DONE, just print it */
	    break;
	  case FRAME_CMD_RESET:
	    /* STATE: RESET */
	    goto_reset();
	    break;
	  default:
	    /* repeat sending histogram */
	    send_histogram(PACKET_HISTOGRAM_RESEND);
	    break;
	  }
	}
      } else if (bit_is_set(UCSR0A, RXC0)) {
	/* there is a character in the UART input buffer */
	const char ch = uart_getc();
	const frame_cmd_t cmd = ch;
	switch (cmd) {
	case FRAME_CMD_ABORT:
	  cli();
	  send_histogram(PACKET_HISTOGRAM_ABORTED);
	  /* STATE: RESET */
	  goto_reset();
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
	  /* NEXT_STATE: MEASURING */
	  break;
	case FRAME_CMD_STATE:
	  /* remain in current state MEASURING, just print it */
	  break;
	default:
	  /* ignore all other bytes */
	  /* NEXT_STATE: MEASURING */
	  break;
	}
	send_state("MEASURING");
      }
    }
}

/** @} */
