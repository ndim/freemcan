/** \file firmware/perso-adc-int-mca-timed-trig.c
 * \brief Personality: MCA with internal ADC and timed trigger
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
 * \defgroup perso_adc_int_mca_timed_trig Personality: MCA with internal ADC and timed trigger
 * \ingroup firmware_personality_groups
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


#include "global.h"
#include "perso-adc-int-global.h"
#include "packet-comm.h"
#include "table-element.h"
#include "data-table.h"
#include "wdt-softreset.h"
#include "timer1-adc-trigger.h"
#include "main.h"


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
  ELEMENT_SIZE_IN_BYTES
};


/** See * \see data_table */
PERSONALITY("adc-int-mca-timed",
            2,2,
            10,
            sizeof(table),
            ELEMENT_SIZE_IN_BYTES);


/** AD conversion complete interrupt entry point
  *
  * This function is called when an A/D conversion has completed.
  * Downsampling of base analog samples and update of histogram table.
  * Actually one could implement a low pass filter here before
  * downsampling to fullfill shannons sample theoreme
  */
ISR(ADC_vect)
{
  /* downsampling of analog data via skip_samples */
  if (skip_samples == 0) {
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
    skip_samples = orig_skip_samples;
  } else {
    skip_samples--;
  }

  /* measurement duration */
  if (GF_IS_CLEARED(GF_MEASUREMENT_FINISHED)) {
    timer1_count--;
    if (timer1_count == 0) {
      GF_SET(GF_MEASUREMENT_FINISHED);
    }
  }

  /** \todo really necessary? */
  /* Clear interrupt flag of timer1 compare match A & B manually since there is no
     TIMER1_COMPB_vect ISR executed                                      */
  TIFR1 |= _BV(OCF1B);
  //TIFR1 |= _BV(OCF1A);
}


/** Do nothing */
void on_measurement_finished(void)
{
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
