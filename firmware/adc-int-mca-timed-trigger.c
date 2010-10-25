/** \file firmware/adc-int-mca-timed-trigger.c
 * \brief Internal ADC based MCA (triggered by timer)
 *
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
 * \defgroup adc_int_mca_timed Internal ADC based MCA (timer triggered)
 * \ingroup firmware
 *
 * Internal ADC code.
 *
 * @{
 */


#include <stdlib.h>

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>


/** Histogram element size */
#define ELEMENT_SIZE_IN_BYTES 3

/** Make sure we use the sub 1 second timer resolution */
#define TIMER_SUB_1SEC


#include "global.h"
#include "adc-int-global.h"
#include "packet-comm.h"
#include "table-element.h"
#include "data-table.h"


/** \bug adc-int-mca-timed does not work. Measurements lead to a reboot. */


/** Number of elements in the histogram table */
#define MAX_COUNTER (1<<ADC_RESOLUTION)


/** Histogram table
 *
 * ATmega644P has 4Kbyte RAM.  When using 10bit ADC resolution,
 * MAX_COUNTER==1024 and 24bit values will still fit (3K table).
 *
 * For the definition of sizeof_table, see adc-int-histogram.c.
 *
 * \see data_table
 */
volatile table_element_t table[MAX_COUNTER] asm("data_table");


/** See * \see data_table */
data_table_info_t data_table_info = {
  /** Actual size of #data_table in bytes */
  sizeof(table),
  /** Type of value table we send */
  VALUE_TABLE_TYPE_HISTOGRAM,
  /** Table element size */
  ELEMENT_SIZE_IN_BYTES,
  /** Total number of elements in table */
  sizeof(table)/ELEMENT_SIZE_IN_BYTES
};


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


/** Initialize IO pins
 */
void io_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void io_init(void)
{
  /* nothing */
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
  if (orig_timer_count == timer_multiple) {
      /* Read analog value */
      uint16_t result = ADCW;
      /* cut off 2, 1 or 0 LSB */
      const uint16_t index = result >> (10-ADC_RESOLUTION);
      /* For 24bit values, the source code looks a little more complicated
       * than just table[index]++ (even though the generated machine
       * instructions are not).  Anyway, we needed to move the increment
       * into a properly defined _inc function.
       */
       volatile table_element_t *element = &(table[index]);
       table_element_inc(element);
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


/** Do nothing */
void on_measurement_finished(void)
{
}


/* just return the sample rate */
inline static
uint16_t get_duration(void)
{
  return orig_timer_count;
}


void all_init(void)
  __attribute__((naked))
  __attribute__((section(".init7")));
void all_init(void)
{
  io_init();
  adc_init();
}


void startup_messages(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init8")));
void startup_messages(void)
{
  send_text_P(PSTR("adc-int-mca-timed-trigger"));
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
