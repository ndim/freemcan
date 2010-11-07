/** \file firmware/adc-int-timed-sampling.c
 * \brief Internal ADC based timed ADC sampling
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
 * \defgroup adc_int_timed_sampling Internal ADC based timed ADC sampling
 * \ingroup firmware
 *
 * Internal ADC based timed ADC sampling.
 *
 * @{
 */


#include <stdlib.h>

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>


/** Histogram element size */
#define ELEMENT_SIZE_IN_BYTES 2

/** Make sure we use the sub 1 second timer resolution */
#define TIMER1_SUB_1SEC


#include "global.h"
#include "adc-int-global.h"
#include "packet-comm.h"
#include "table-element.h"
#include "data-table.h"
#include "timer1-adc-trigger.h"
#include "main.h"


/* forward declaration */
inline static
void timer1_halt(void);


/** The table
 *
 * Note that we have the table location and size determined by the
 * linker script time-series-table.x.
 */
extern volatile table_element_t table[] asm("data_table");


/** End of the table: Still needs rounding */
extern volatile table_element_t data_table_end[];


/** Pseudo symbol - just use its address */
extern volatile char data_table_size[];


/** Data table info
 *
 * \see data_table
 */
data_table_info_t data_table_info = {
  /** Actual size of #data_table in bytes
   * We update this value whenever new time series data has been
   * recorded. The initial value is "zero bytes" (elements).
   */
  0,
  /** Type of value table we send */
  VALUE_TABLE_TYPE_SAMPLES,
  /** Table element size */
  ELEMENT_SIZE_IN_BYTES
};


/** See * \see data_table */
PERSONALITY("adc-int-timed-sampling",
            0,2,
            10,
            ((size_t)(&data_table_size)),
            ELEMENT_SIZE_IN_BYTES);


/** End of the table: Never write to *table_cur when (table_cur>=table_end)! */
volatile table_element_t *volatile table_end =
  (table_element_t volatile *)((char *)data_table_end -
                                   (sizeof(table_element_t)-1));

/** Pointer to the current place to store the next value at */
volatile table_element_t *volatile table_cur = table;


/** Setup, needs to be called once on startup
 *
 * \bug (copied from geiger-time-series)
 */
void ts_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void ts_init(void)
{
  /** As the table is outside of the memory area with the normal data,
   * its content will NOT be cleared by the default avr-libc startup
   * code.  So we clear the table memory ourselves.
   */
  asm volatile("\t /* assembly code taken from GPLv2+ libgcc.S __do_clear_bss */ \n"
               "\t	ldi     r17, hi8(data_table_end)\n"
               "\t	ldi     r26, lo8(data_table)\n"
               "\t	ldi     r27, hi8(data_table)\n"
               "\t	rjmp    L%=_start\n"
               "\tL%=_loop:\n"
               "\t	st      X+, __zero_reg__\n"
               "\tL%=_start:\n"
               "\t	cpi     r26, lo8(data_table_end)\n"
               "\t	cpc     r27, r17\n"
               "\t	brne    L%=_loop\n"
               ::
               );
}


/** Print some status messages for debugging
 *
 * \bug (copied from geiger-time-series.c)
 */
void ts_print_status(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init8")));
void ts_print_status(void)
{
#ifdef VERBOSE_STARTUP_MESSAGES
  uprintf("<ts_print_status>");
  uprintf("%-25s %p", "table",      table);
  uprintf("%-25s %p", "table_cur",  table_cur);
  uprintf("%-25s %p", "table_end",  table_end);
  const size_t UV(sizeof_table) = ((char*)table_end) - ((char*)table_cur);
  uprintf("%-25s 0x%x = %d >= %d * %d",
          "table_end - table_cur",
          _UV(sizeof_table), _UV(sizeof_table),
          _UV(sizeof_table)/sizeof(*table_cur), sizeof(*table_cur));
  uprintf("</ts_print_status>");
#endif
}


/** AD conversion complete interrupt entry point
 *
 * This function is called when an A/D conversion has completed.
 * Downsampling of base analog samples and update of sample table.
 * Actually one could implement a low pass filter here before
 * downsampling to fullfill shannons sample theoreme
 */
ISR(ADC_vect)
{
  /* downsampling of analog data as a multiple of timer1_multiple      */
  const uint16_t result = ADCW;
  if (skip_samples == 0) {
    /* Read analog value */
    if (!measurement_finished) {
      /* Write to current position in table */
      *table_cur = result >> (10-ADC_RESOLUTION);
      table_cur++;
      data_table_info.size += sizeof(*table_cur);
      skip_samples = orig_skip_samples;
      if (table_cur >= table_end) {
        /* switch off any compare matches on B to stop sampling     */
        timer1_halt();
        /* tell main() that measurement is over                     */
        measurement_finished = 1;
      }
    }
  } else {
    skip_samples--;
  }

  /** \todo really necessary? */
  /* Clear interrupt flag of timer1 compare match B manually since there is no
     TIMER1_COMPB_vect ISR executed but the ADC is triggered on rising edge
     of interrupt flag                                                */
  TIFR1 |= _BV(OCF1B);
  //TIFR1 |= _BV(OCF1A);
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


/** Switch off trigger B to stop any sampling of the analog signal
 *
 *
 */
inline static
void timer1_halt(void)
{
  const uint8_t old_tccr1b = TCCR1B;
  /* pause the clock */
  TCCR1B &= ~(_BV(CS12) | _BV(CS11) | _BV(CS10));
  /* Switch off trigger B to avoid an additional sampling of data */
  OCR1B = TIMER1_COMPARE_MATCH_VAL+1;
  /* blinking for measurement is over */
  OCR1A = TIMER1_COMPARE_MATCH_VAL;
  /* start counting from 0, needs clock to be paused */
  TCNT1 = 0;
  /* unpause the clock */
  TCCR1B = old_tccr1b;
}


/** Reconfigure Timer to show the user that the measurement has been finished
 *
 *
 */
inline static
void timer1_init_quick(void)
{
  const uint8_t old_tccr1b = TCCR1B;
  /* pause the clock */
  TCCR1B &= ~(_BV(CS12) | _BV(CS11) | _BV(CS10));
  /* Switch off trigger B to avoid an additional sampling of data */
  OCR1B = TIMER1_COMPARE_MATCH_VAL_MEASUREMENT_OVER+1;
  /* blinking for measurement is over */
  OCR1A = TIMER1_COMPARE_MATCH_VAL_MEASUREMENT_OVER;
  /* start counting from 0, needs clock to be paused */
  TCNT1 = 0;
  /* unpause the clock */
  TCCR1B = old_tccr1b;
}


/** Callback */
void on_measurement_finished(void)
{
  /* alert user */
  timer1_init_quick();
}


void all_init(void)
  __attribute__((naked))
  __attribute__((section(".init7")));
void all_init(void)
{
  adc_init();
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
