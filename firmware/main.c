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

/* following two includes need F_CPU */
#include "ad7813.h"
#include <util/delay.h>

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


/** Interrupt 0 service proceeding PMT pulse with external ADC
 *
 * Softtrigger of the AD-converter
 * Get data
 * Update of table
 * Reset of sample & hold capacitor
 *
 * Timing of the following code which depends on F_CPU is crucial
 * since all hardware functionality is emulated by software!
 *
 * Measurements done with F_CPU = 18,432 MHz and five NOPs (AD7813_DELAY)
 * 9-Bit ADC resoluteion and 24 bit table:
 * Falling edge INT0 til falling edge CONVST         : 1700ns
 * Pulse width CONVST is on GND                      : 700ns
 * Rising edge BUSY til rising edge CONVST           : 700ns
 * Pulse width BUSY is on VDD                        : 1700ns
 * 1st data read out - READ is on GND                : 700ns
 * READ is on VDD                                    : 400ns
 * snd data read out - READ is on GND (second pulse) : 700ns

 * Dead time (complete runtime) should be approx. 10,8us if
 * preamp is populated with 27pF || 120kOhm
 */
//__attribute__((optimize(3))) ISR(INT0_vect){
ISR(INT0_vect){
   /* Wait til preamplifier, sample & hold circuit and ADC input are
    * settled down and stable before starting a software trigger on
    * the ADC. The interrupt execution response is five clock cycles minimum.
    * After five clock cycles the program vector address for the
    * actual interrupt handling routine is executed. During these five
    * clock cycle period, the program counter (three bytes) is pushed
    * onto the stack. The vector is a jump to the interrupt routine,
    * and takes three clock cycles. Some working registers are pushed
    * onto the stack.
    *
    * Force falling edge on CONVST pin to soft trigger an ADC conversion
    * (track and hold)
    */
   AD7813_IO_CONVST_PORT &= ~_BV(AD7813_IO_CONVST_CTRL_BIT);
   /* 1.) Wait for at least AD7813_T_CONVST_FALLING_EDGE_TO_BUSY_RISING_EDGE_DELAY
    * (t3) but less than (AD7813_T_CONVERSION [t1] -
    * AD7813_T_CONVST_FALLING_EDGE_TO_BUSY_RISING_EDGE_DELAY [t3])
    * to have a stable busy signal from AD7813
    * 2.) Before rising the CONVST signal back to VDD (reset) you have
    * to wait for AD7813_T_CONVST_PULSEWIDTH (t2) to provide a minimum
    * pulse width to GND on the CONVST PIN. The CONVST must be reset
    * to VDD to operate AD7813 in the fast MODE 1 but this must happen
    * during BUSY is high
    * Condition 1.) is fullfilled if 30ns < debounce time < 2270ns
    * Condition 2.) is fullfilled if 20ns < debounce time
    */
   AD7813_DELAY
   AD7813_DELAY
   /* Reset CONVST to VDD during BUSY is high to operate ADC in MODE 1 */
   AD7813_IO_CONVST_PORT |= (_BV(AD7813_IO_CONVST_CTRL_BIT));
   /* Poll BUSY signal til AD conversion is completed (falling edge on
    * BUSY signal) */
   loop_until_bit_is_clear(AD7813_IO_BUSY_PIN, AD7813_IO_BUSY_CTRL_BIT);

   /* Read 1byte result from ADC: force READ to GND */
   AD7813_IO_RD_PORT &= ~(_BV(AD7813_IO_RD_CTRL_BIT));
   /* Wait for at least AD7813_T_DATA_ACCESS_TIME_AFTR_RD_LOW (t6) til
    * data on latch is valid */
   AD7813_DELAY
   /* Read higher byte (8 bit) from ADC latch */
   register uint8_t result1 = AD7813_IO_DATA_PIN;
   /* Wait til atmega port is stable */
   AD7813_DELAY
   /* Force READ pin to VDD (reset) */
   AD7813_IO_RD_PORT |= (_BV(AD7813_IO_RD_CTRL_BIT));

   /* 9 or 10 bit: Serial read out of lower two bits. Not so fast but
    * higher resolution. Wait for at least either
    * AD7813_T_BUS_RELINQUISH_TIME_AFTR_RD_HIGH (t7) or
    * AD7813_T_MIN_BETWEEN_MSB_AND_LSB_READS (t8) */
   /* #if (ADC_RESOLUTION > 8) */
   AD7813_DELAY
   /* Force READ to GND to access lower 2 Bits by serial data read out */
   AD7813_IO_RD_PORT &= ~(_BV(AD7813_IO_RD_CTRL_BIT));
   AD7813_DELAY
   /* Read lower byte (8 bit) from ADC latch */
   register uint8_t result0 = AD7813_IO_DATA_PIN;
   /* Wait til ATmega latch is stable */
   AD7813_DELAY
   /* Force READ to VDD to reset the signal */
   AD7813_IO_RD_PORT |= (_BV(AD7813_IO_RD_CTRL_BIT));

   const uint16_t index = (((((uint16_t)(result1)) << 2) | (result0 >> 6)) >> (10-ADC_RESOLUTION));

   /* 8 bit or less (delay must be adjusted separately)
   #else
     const uint16_t index = ((uint16_t)(result1) >> (8-ADC_RESOLUTION));
   #endif */

   /* The ADC needs a pause time of
    * AD7813_T_RISING_EDGE_OF_CS_OR_RD_TO_FALLING_EDGE_OF_CONVST_DELAY
    * now (t9) which is far below the rest of this ISR so we do not
    * care about it */

   /* For 24bit values, the source code looks a little more complicated
    * than just table[index]++ (even though the generated machine
    * instructions are not).  Anyway, we needed to move the increment
    * into a properly defined _inc function.
    */
   volatile histogram_element_t *element = &(table[index]);
   histogram_element_inc(element);

   /* Wait til preamp falls below INT0 trigger threshold
    * with 27pF||120k */
   _delay_us(AD7813_TO_SHRST_T_DELAY);

   /* Reset SH-CAP - 2,2us pulse width, start approx 9us after INT0 */
   SHRST_IO_PORT |= _BV(SHRST_IO_CTRL_BIT);
   _delay_us(2);
   /* Set pin to GND and release peak hold capacitor   */
   SHRST_IO_PORT &=~ _BV(SHRST_IO_CTRL_BIT);

   /* If there are any pending int0 interrupts clear them since they cannot
    * be valid (e.g. second PMT pulse occured during execution of ISR) */
   EIFR |= _BV(INTF0);
}

/** 16 Bit timer ISR
 *
 * When timer has elapsed, the global #timer_flag (8bit, therefore
 * atomic read/writes) is set.
 *
 * Note that we are counting down the timer_count, so it will start
 * with its maximum value and count down to zero.
 *
 * \see last_timer_count, get_duration
 */
ISR(TIMER1_COMPA_vect)
{
  /* toggle a sign PORTD ^= _BV(PD5); (done automatically) */

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



/** Get elapsed time in the currently running or finished measurement
 *
 * We do synchronized reading of the multi-byte variable timer_count
 * here (which is being written to by the ISR(TIMER1_COMPA_vect) while
 * send_histogram() is being executed for 'I' histograms). For all other
 * types of histograms, this busy sync loop will still work, but is not
 * required as all interrupts will be disabled.
 *
 * Reading both #timer_count and #last_timer_count consists basically of
 * the following four steps:
 *
 *   a) read lo8(#timer_count)
 *
 *   b) read hi8(#timer_count)
 *
 *   c) read lo8(#last_timer_count)
 *
 *   d) read hi8(#last_timer_count)
 *
 * Now we have a finite set of sequences in which those instructions
 * and ISR(TIMER1_COMPA_vect) can be executed in relation to each
 * other (keep in mind that the #timer_count is counted backwards):
 *
 *  1. ISR before a): No problem whatsoever.  #last_timer_count will
 *     be 1 more than #timer_count.  The while loop will finish.
 *
 *  2. ISR between a) and b): #timer_count will differ from
 *     #last_timer_count by two, or a lot more in the case of a 8bit
 *     overflow happening.  The while loop will continue.
 *
 *  3. ISR between b) and c): #timer_count will be the same as
 *     #last_timer_count.  The while loop will continue.
 *
 *  4. ISR between c) and d): #timer_count will be the same as
 *     #last_timer_count (just like case 3), or a lot more in the case
 *     of a 8bit overflow happening.  The while loop will continue.
 *
 *  5. ISR after d): No problem whatsoever.  #last_timer_count will be
 *     1 more than #timer_count.  The while loop will finish.
 *
 * As the ISR runs only every second, and we can reasonably presume
 * that the while loop can repeats a number of times during that
 * second, this will terminate quite quickly with a useful result.
 *
 * The result may be off by one for the 'I' histograms but not by
 * more, and for 'I' results we can tolerate that kind of inaccuracy.
 *
 * Durations for finished measurements will always be accurate, as
 * that will trigger case 1.
 *
 * \see last_timer_count, ISR(TIMER1_COMPA_vect)
 */
inline static
uint16_t get_duration(void)
{
  uint16_t a, b;
  do {
    a = timer_count;
    b = last_timer_count;
  } while ((b-a) != 1);
  /* Now 'a' contains a valid value. Use it. */
  const uint16_t duration = orig_timer_count - a;
  return duration;
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
    DDRD &= ~(_BV(DDD2));
    /* Port D data register: Enable pull up on pin 16, 20-50kOhm */
    PORTD |= _BV(PD2);

    /* Disable interrupt "INT0" (clear interrupt enable bit in
     * external interrupt mask register) otherwise an interrupt may
     * occur during level and edge configuration (EICRA)  */
    EIMSK &= ~(_BV(INT0));
    /* Level and edges on the external pin that activates INT0
     * is configured now (interrupt sense control bits in external
     * interrupt control register A). Disable everything.  */
    EICRA &= ~(_BV(ISC01) | _BV(ISC00));
    /* Now enable interrupt on falling edge.
     * [ 10 = interrupt on rising edge
     *   11 = interrupt on falling edge ] */
    EICRA |=  _BV(ISC01);
    /* Clear interrupt flag by writing a locical one to INTFn in the
     * external interrupt flag register.  The flag is set when a
     * interrupt occurs. if the I-flag in the sreg is set and the
     * corresponding flag in the EIFR the program counter jumps to the
     * vector table  */
    EIFR |= _BV(INTF0);
    /* reenable interrupt INT0 (External interrupt mask
     * register). jump to the ISR in case of an interrupt  */
    EIMSK |= (_BV(INT0));

}


/** Configure 16 bit timer to trigger an ISR every second
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

  /* Prescaler settings on timer conrtrol reg. B                  */
  TCCR1B |=  ((((TIMER_PRESCALER >> 2) & 0x1)*_BV(CS12)) |
              (((TIMER_PRESCALER >> 1) & 0x1)*_BV(CS11)) |
              ((TIMER_PRESCALER & 0x01)*_BV(CS10)));

  /* Compare match value into output compare reg. A               */
  OCR1A = TIMER_COMPARE_MATCH_VAL;

  /* output compare match A interrupt enable                      */
  TIMSK1 |= _BV(OCIE1A);
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
  TCCR1B &= ~(_BV(CS12) | _BV(CS11) | _BV(CS10));
  /* faster blinking */
  OCR1A = TIMER_COMPARE_MATCH_VAL / 4;
  /* start counting from 0, needs clock to be paused */
  TCNT1 = 0;
  /* unpause the clock */
  TCCR1B = old_tccr1b;
}

/** Initialize AD7813 peripherals
 */
inline static
void AD7813_init(void)
{
    /* ADC7813 data bus */

    /* Configure complete port group as input */
    AD7813_IO_DATA_DDR &= ~AD7813_IO_DATA_CTRL_VALUE;
    /* Disable all internal pull ups on this port, Isource=Isink=200uA */
    AD7813_IO_DATA_PORT &= ~AD7813_IO_DATA_CTRL_VALUE;

    /* Configure ADC control pins. Inputs first of all to avoid
     * short circuits */

    /* Configure BUSY as input */
    AD7813_IO_BUSY_DDR &= ~(_BV(AD7813_IO_BUSY_CTRL_BIT));
    /* Disable pull up resistor at input */
    AD7813_IO_BUSY_PORT &= ~_BV(AD7813_IO_BUSY_CTRL_BIT);

    /* Configure READ as output */
    AD7813_IO_RD_DDR |= (_BV(AD7813_IO_RD_CTRL_BIT));
    /* Set READ to VDD */
    AD7813_IO_RD_PORT |= (_BV(AD7813_IO_RD_CTRL_BIT));

    /* Configure CONVST as output */
    AD7813_IO_CONVST_DDR |= (_BV(AD7813_IO_CONVST_CTRL_BIT));
    /* Set CONVST to VDD */
    AD7813_IO_CONVST_PORT |= (_BV(AD7813_IO_CONVST_CTRL_BIT));

    /* An internal INT CONVST signal pulse is started after CONVST
     * is switched to VDD. After the ADC is powered up and INT
     * CONVST goes down the ADC is engaged and ready for track/hold
     * edge on CONVST. The power up takes a time of
     * AD7813_T_PWR_UP_AFTR_RISING_EDGE_ON_CONVST. So actually we
     * need to wait at this point for that time but it is small and
     * so we assume that all following code consumes more time  */
}

/** Initialize peripherals
 *
 * Configure peak hold capacitor reset pin
 * Configure unused pins
 */
inline static
void io_init(void)
{
    /* configure pin as an output                               */
    SHRST_IO_DDR |= (_BV(SHRST_IO_CTRL_BIT));
    /* set pin to ground                                        */
    SHRST_IO_PORT &= ~_BV(SHRST_IO_CTRL_BIT);

    /** \todo configure unused pins */
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

    AD7813_init();

    /* configure INT0 pin 16 */
    trigger_src_conf();

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
          timer_init_quick();
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
