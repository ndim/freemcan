/** \file firmware/perso-geiger-time-series.c
 * \brief Personality: Geiger Counter Recording Time Series
 *
 * \author Copyright (C) 2010 samplemaker
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
 * \author Copyright (C) 1998, 1999, 2000, 2007, 2008, 2009 Free Software Foundation, Inc.
 *         (for the assembly code in ts_init() to clear data_table)
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
 * \defgroup geiger_time_series Personality: Geiger Counter Recording Time Series
 * \ingroup firmware_personality_groups
 *
 * Geiger Counter recording time series
 *
 * @{
 */


#include <stdlib.h>
#include <string.h>


#include <avr/io.h>
#include <avr/interrupt.h>


/** Histogram element size */
#define ELEMENT_SIZE_IN_BYTES 1


#include "compiler.h"
#include "global.h"
#include "packet-comm.h"
#include "timer1-measurement.h"
#include "uart-printf.h"
#include "main.h"
#include "table-element.h"
#include "data-table.h"
#include "beep.h"

#ifndef F_CPU
# error Need F_CPU defined for util/delay.h
#endif
#include <util/delay.h>


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
   * recorded. The initial value is "one element".
   */
  sizeof(table[0]),
  /** Type of value table we send */
  VALUE_TABLE_TYPE_TIME_SERIES,
  /** Table element size */
  ELEMENT_SIZE_IN_BYTES
};


/** See * \see data_table */
PERSONALITY("geiger-time-series",
            2,0,
            1,
            ((size_t)(&data_table_size)),
            ELEMENT_SIZE_IN_BYTES);


/** End of the table: Never write to *table_cur when (table_cur>=table_end)! */
volatile table_element_t *volatile table_end =
  (table_element_t volatile *)((char *)data_table_end -
                                   (sizeof(table_element_t)-1));

/** Pointer to the current place to store the next value at */
volatile table_element_t *volatile table_cur = table;


/** Print some status messages for debugging */
void data_table_print_status(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init8")));
void data_table_print_status(void)
{
#ifdef VERBOSE_STARTUP_MESSAGES
  uprintf("<data_table_print_status>");
  uprintf("%-25s %p", "table",      table);
  uprintf("%-25s %p", "table_cur",  table_cur);
  uprintf("%-25s %p", "table_end",  table_end);
  const size_t UV(sizeof_table) = ((char*)table_end) - ((char*)table_cur);
  uprintf("%-25s 0x%x = %d >= %d * %d",
          "table_end - table_cur",
          _UV(sizeof_table), _UV(sizeof_table),
          _UV(sizeof_table)/sizeof(*table_cur), sizeof(*table_cur));
  uprintf("</data_table_print_status>");
#endif
}


/** Initialize peripherals
 *
 * Set up output pins to Pollin Eval Board speaker and LED2.
 */
void personality_io_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void personality_io_init(void)
{
  /* configure pin 20 as an output                               */
  DDRD |= (_BV(DDD4));
  PORTD &= ~_BV(PD4);
}


/** External INT0, i.e. count a GM event */
ISR(INT0_vect)
{
  /* activate loudspeaker */
  _beep();
  /* toggle output pin with LED */
  PIND |= _BV(PD4);

  if (table_cur < table_end) {
    table_element_inc(table_cur);
  }
  /* debounce any pending unwanted interrupts caused bouncing
     during transition */
  EIFR |= _BV(INTF0);
}


volatile uint16_t timer1_count;
volatile uint16_t orig_timer1_count;


ISR(TIMER1_COMPA_vect)
{
  /* toggle a sign PORTD ^= _BV(PD5); (done automatically) */

  if (GF_IS_CLEARED(GF_MEASUREMENT_FINISHED)) {
    /** We do not touch the measurement_finished flag ever again after
     * setting it. */
    timer1_count--;
    if (timer1_count == 0) {
      /* Timer has elapsed. Advance to next counter element in time
       * series, and restart the timer countdown. */
      table_cur++;
      if (table_cur < table_end) {
        data_table_info.size += sizeof(*table_cur);
        timer1_count = orig_timer1_count;
      } else {
        GF_SET(GF_MEASUREMENT_FINISHED);
      }
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
    DDRD &= ~(_BV(DDD2));
    /* Port D data register: Enable pull up on pin 16, 20-50kOhm */
    PORTD &= ~_BV(PD2);

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
     * register). we do not want to jump to the ISR in case of an interrupt
     * so we do not set this bit  */
    EIMSK |= (_BV(INT0));
}


void on_measurement_finished(void)
{
  beep_kill_all();
  timer1_init_quick();
}


void personality_start_measurement_sram(void)
{
  const void *voidp = &pparam_sram.params[0];
  const uint16_t *timer1_value = voidp;
  trigger_src_conf();
  timer1_init(*timer1_value);
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
