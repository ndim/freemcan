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
 * @{
 */

/*------------------------------------------------------------------------------
 * Includes
 *------------------------------------------------------------------------------
 */

#include <avr/io.h>
#include <avr/interrupt.h>
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
volatile uint16_t timer_count = 0;
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
volatile uint8_t timer_flag = 0;


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

  /** \todo Can the ADC return values exceeding 8, 9, 10bit? Probably not. */
  /** \todo Reconcile ADC_RESOLUTION, MAX_COUNTER, table index, etc. */
  /* Update histogram: this can be a 8 or a 9 or a 10 bit index! */

  /* cut off 2, 1 or 0 LSB */
  const uint16_t index = result >> (10-ADC_RESOLUTION);

  /* For 24bit values, this looks a little more complicated than just
   * table[index]++, so we have moved the increment into the _inc function.
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
    PORTD &= ~ BIT(PD6);

    /** \todo configure unused pins */
}


/** Send histogram packet to controller via serial port (layer 3).
 *
 * \param type The type of histogram we are sending
 *             (#packet_histogram_type_t).  You may also a dummy value
 *             like '?' or -1 or 0xff or 0 until you make use of that
 *             value on the receiver side.
 *
 * \bug Disable timer counting while we are busy sending data and thus
 *      not measuring.
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

  packet_histogram_header_t header = {
    ELEMENT_SIZE_IN_BYTES,
    type,
    orig_timer_count - a
  };
  frame_start(FRAME_TYPE_HISTOGRAM, sizeof(header)+sizeof(table));
  uart_putb((const void *)&header, sizeof(header));
  uart_putb((const void *)table, sizeof(table));
  frame_end();
}


/** Send status message packet to host (layer 3).
 *
 * Status messages are constant strings.
 */
static
void send_status(const char *msg)
{
  const size_t len = strlen(msg);
  frame_send(FRAME_TYPE_STATUS, msg, len);
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

#if 0
    /* All of BSS is initialized to zero in the startup code, so we only
     * need to initialize variables whose value differs from "all zero
     * bytes".
     */
    /* Initialize global variables */
    max_timer_count = 0;
    max_timer_flag  = 0;
    for (int i=0; i<MAX_COUNTER; i++) {
      table[i]=0;
    }
#endif

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
    send_status("READY");

    uint8_t quit_flag = 0;
    uint16_t timer_value;
    while (!quit_flag) {
      uart_checksum_reset();
      const char ch = uart_getc();
      const frame_cmd_t cmd = ch;
      switch (cmd) {
      case FRAME_CMD_RESET:
	send_status("RESET");
	/* STATE: RESET */
	soft_reset();
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
	    send_status("MEASURING");
	    /* NEXT_STATE: MEASURING */
	    quit_flag = 1;
	  } else { /* checksum fail */
	    send_status("CHKSUMFAIL");
	    /* STATE: RESET */
	    soft_reset();
	  }
	}
	break;
      default:
	/* ignore all other bytes */
	/* NEXT_STATE: READY */
	break;
      }
    }

    /* STATE: MEASURING */

    /* set up timer with the value we just got */
    timer_count = orig_timer_count = timer_value;

    /* begin measurement */
    timer_init();
    sei();

    while (1) {
      if (timer_flag) { /* done */
	cli();
	send_histogram(PACKET_HISTOGRAM_DONE);
	soft_reset();
      } else if (bit_is_set(UCSR0A, RXC0)) {
	/* there is a character in the UART input buffer */
	const char ch = uart_getc();
	const frame_cmd_t cmd = ch;
	switch (cmd) {
	case FRAME_CMD_ABORT:
	  cli();
	  send_histogram(PACKET_HISTOGRAM_ABORTED);
	  /* STATE: RESET */
	  soft_reset();
	break;
	case FRAME_CMD_INTERMEDIATE:
	  cli();
	  send_histogram(PACKET_HISTOGRAM_INTERMEDIATE);
	  /** \bug reset peak-hold capacitor before resuming proper measuring */
	  sei();
	  /* NEXT_STATE: MEASURING */
	  break;
	default:
	  /* ignore all other bytes */
	  /* NEXT_STATE: MEASURING */
	  break;
	}
      }
    }

}

/** @} */
