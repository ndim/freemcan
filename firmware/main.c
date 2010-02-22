/** \file main.c
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

/* Number of elements in the histogram table */
#define MAX_COUNTER (1<<ADC_RESOLUTION)

#define BIT(NO) (1<<(NO))


/** Trigger AVR reset via watchdog device. */
#define soft_reset()						\
  do {								\
    wdt_enable(WDTO_15MS);					\
    while (1) {							\
      /* wait until watchdog has caused a systems reset */	\
    }								\
  } while(0)


/*------------------------------------------------------------------------------
 * Variables  (static, not visible in other modules)
 *------------------------------------------------------------------------------
 */

volatile uint16_t timer_msec = 0;

/** histogram table */
volatile uint32_t table[MAX_COUNTER];

/** count number of measurements
 *
 * Use 8 bit value to ensure atomic read/write operations.
 */
volatile uint8_t measurement_count;


/** timer counter
 *
 * Written by main() with value received from controller.
 * Read and written by timer interrupt handler.
 */
volatile uint16_t timer_count = 0;


/** Timer counter has reached zero.
 *
 * Will be set to 1 when max_timer_count is exceeded, is 0 otherwise.
 * Written by timer interrupt handler.
 * Read by main loop.
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


/*
ISR(INT0_vect) {
}*/


/** AD conversion complete interrupt entry point
 *
 * This function is called when an A/D conversion has completed.
 */
ISR(ADC_vect) {

  uint16_t result;
  uint8_t index;

  result = ADCW;

  index = (uint8_t)(result>>(10-ADC_RESOLUTION));

  /* Update histogram */
  table[index]++;

  /* Update measurement counter */
  measurement_count++;

  /* If a hardware event on int0 pin occurs an interrupt flag in EIFR is set.
   * Since int0 is only configured but not enabled ISR(INT0_vect){} is
   * not executed and therefore this flag is not reset automatically.
   * To reset this flag the bit at position INTF0 must be set.
   */
  EIFR |= BIT(INTF0);
}

/** 16 Bit timer ISR
 *
 *  If timer is elapsed the global flag timer_flag is set
 */
ISR (TIMER1_COMPA_vect)
{
  /* toggle a sign: PORTD ^= BIT(PD5);                          */
  /* if the timer is elapsed set a flag to control main program */
  if (!timer_flag) {
    timer_count--;
    /* if the timer count multiples are over set a 8 bit flag   */
    if (timer_count == 0) {
       timer_flag = 1;
    }
  }
}

/** Setup of INT0
 *
 * INT0 Pin 16 is configured but not enabled
 * On rising edge
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

    /* Disable interrupt pin "INT0" (clear interrupt enable bit in
     * external interrupt mask register) otherwise an interrupt may
     * occur if EICRA is changed */
    EIMSK &= ~(BIT(INT0));
    /* Int on rising or falling edge or level triggered (external
     * interrupt control register A) */
    EICRA &= ~(BIT(ISC01) | BIT(ISC00));
    /* 11 = interrupt on rising edge (setze bit 0 und 1 auf 1) */
    EICRA |=  (BIT(ISC01) | BIT(ISC00));
    /* Clear interrupt flag by writing a locical one to INTFn in the
     * external interrupt flag register.  The flag is set when a
     * interrupt occurs. if the I-flag in the sreg is set and the
     * corresponding flag in the EIFR the program counter jumps to the
     * vector table*/
    EIFR |= BIT(INTF0);
    /* reenable interrupt INT0 (External interrupt mask
     * register). we do not want to jump to the ISR in case of an interrupt
     * so we do not set this bit                               */
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


/** Configure 16 bit timer to trigger a ISR each 1 second         */
static
void timer_init(void){

  /* Prepare timer 0 control register A for
     clear timer on compare match (CTC)                           */
  TCCR1A = 0;

  TCCR1B =  BIT(WGM12);

  /* Prescaler settings on timer conrtrol reg. B                  */
  TCCR1B |=  ((((TIMER_PRESCALER >> 2) & 0x1)*BIT(CS12)) |
              (((TIMER_PRESCALER >> 1) & 0x1)*BIT(CS11)) |
              ((TIMER_PRESCALER & 0x01)*BIT(CS10)));

  /* Compare match value into output compare reg. A               */
  OCR1A = TIMER_COMPARE_MATCH_VAL;

  /* output compare match A interrupt enable                      */
  TIMSK1 |= BIT(OCIE1A);
}


/* dig it */
/* inline static
void call_me_every_second(void)
{
  if (!timer_flag) {
    timer_count--;
    if (timer_count == 0) {
      timer_flag = 1;
    }
  }
}*/


/** Send histogram table[] to controller via serial port.
 *
 * \param type The type of histogram we are sending
 *             (#packet_histogram_type_t).  You may also a dummy value
 *             like '?' or -1 or 0xff or 0 until you make use of that
 *             value on the receiver side.
 */
static
void send_histogram(const packet_histogram_type_t type)
{
  frame_start(FRAME_TYPE_HISTOGRAM, sizeof(table)+1+1);
  const uint8_t element_size = sizeof(table[0]);
  uart_putc((const char)(element_size));
  uart_putc((const char)(type));
  uart_putb((const void *)table, sizeof(table));
  frame_end();
}


/** Send status message to host.
 *
 * Status messages are constant strings.
 */
static
void send_status(const char *msg)
{
  const size_t len = strlen(msg);
  frame_send(FRAME_TYPE_STATUS, msg, len);
}


/** Send text message to host.
 *
 * If you need to send more than static text, use uprintf().
 */
static
void send_text(const char *msg)
{
  const size_t len = strlen(msg);
  frame_send(FRAME_TYPE_TEXT, msg, len);
}


/**
 * \todo Implement "set measurement timing" command.
 * \todo Implement communication FSM according to frame-defs.h.
 */

/** AVR firmware's main "loop" function
 *
 * Note that we create a "loop" by resetting the AVR device when we
 * are finished, which will cause the system to start again with the
 * hardware and software in the defined default state.
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
    measurement_count = 0;
    for (int i=0; i<MAX_COUNTER; i++) {
      table[i]=0;
    }
#endif

    /* STATE: BOOT */
    /** \todo configure unused pins */

    /* configure USART0 for 8N1 */
    uart_init();

    /* configure INT0 pin 16 on rising edge */
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
	  const uint8_t byte0 = uart_getc();
	  const uint8_t byte1 = uart_getc();
	  timer_value = (((uint16_t)byte1)<<8) | byte0;
	  if (uart_checksum_recv()) { /* checksum successful */
	    send_status("MEASURING");
	    /* STATE: MEASURING */
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
	break;
      }
    }

    /* STATE: MEASURING */

    /* set up timer with the value we just got */
    timer_count = timer_value;

    /* begin measurement */
    timer_init();
    sei();

    while (1) {
      if (timer_flag) { /* done */
	cli();
	send_histogram(PACKET_HISTOGRAM_DONE);
	soft_reset();
	break;
      } else if (bit_is_set(UCSR0A, RXC0)) {
	/* there is a character in the UART input buffer */
	const char ch = uart_getc();
	const frame_cmd_t cmd = ch;
	switch (cmd) {
	case FRAME_CMD_ABORT:
	  cli();
	  send_histogram(PACKET_HISTOGRAM_ABORTED);
	  soft_reset();
	break;
	case FRAME_CMD_INTERMEDIATE:
	  cli();
	  send_histogram(PACKET_HISTOGRAM_INTERMEDIATE);
	  sei();
	  break;
	default:
	  /* ignore all other bytes */
	  break;
	}
      }
    }

}
