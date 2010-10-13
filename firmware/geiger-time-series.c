/** \file firmware/geiger-time-series.c
 * \brief Geiger Counter recording time series
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
 * \defgroup geiger_time_series Geiger Counter Recording Time Series
 * \ingroup firmware
 *
 * Geiger Counter recording time series
 *
 * @{
 */


#include <stdlib.h>
#include <string.h>


#include <avr/io.h>
#include <avr/interrupt.h>

#include "../hostware/compiler.h"
#include "global.h"
#include "packet-comm.h"
#include "measurement-timer.h"
#include "uart-printf.h"
#include "main.h"


#ifndef F_CPU
# error Need F_CPU defined for util/delay.h
#endif
#include <util/delay.h>


#define DELAY_BEEP 200


/** Maximum expected stack depth in bytes
 *
 * An arbitrary value which should be rooted in reality somehow.
 *
 * \bug Needs a check to prevent overflows and make sure stuff
 *      actually fits into SRAM.
 */
#define MAX_STACK_DEPTH 128


/** Maximum size of area for malloc
 *
 * malloc(3) might be run by some function from <stdio.h>, so if we
 * link with uart-printf.o we might need a MALLOC_AREA_SIZE other than
 * 0.
 *
 * An arbitrary value which should be rooted in reality somehow.
 *
 * \bug Needs a check to prevent overflows and make sure stuff
 *      actually fits into SRAM.
 */
#ifdef HAVE_UPRINTF_IMPLEMENTATION
# define MALLOC_AREA_SIZE 128
#else
# define MALLOC_AREA_SIZE 0
#endif


/** The table
 *
 * Note that we have the table start at the start of the heap,
 * i.e. after all variables. The end of the table will be where the
 * stack ends (stack grows downwards).
 *
 * This has the additional consequence that we MUST NOT use malloc(3),
 * calloc(3), free(3) anywhere in our program.
 */
extern volatile histogram_element_t table[] asm("__heap_start");


/** End of the table: Never write to *table_cur when (table_cur>=table_end)! */
volatile histogram_element_t *table_end;


/** Pointer to the current place to store the next value at */
volatile histogram_element_t *table_cur;


/** Address of data table
 *
 * \see data_table
 */
const void *data_table = &table;


/** Actual size of #data_table in bytes
 *
 * We update this value whenever new time series data has been
 * recorded.
 *
 * \see data_table
 */
size_t sizeof_data_table = sizeof(*table_cur);


/** Setup, needs to be called once on startup */
void ts_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void ts_init(void)
{
  size_t off = (RAMEND) - (MAX_STACK_DEPTH) - (MALLOC_AREA_SIZE)
    - (sizeof(*table_cur)-1);
  table_end = (void *)(off);
  table_cur = table;
  /** The functions from <stdio.h> might be using malloc(3) & Co, so
   * we should make sure that malloc(3) does not overwrite our table
   * (and that we do not overwrite the data structures created by
   * malloc(3)!).
   */
  __malloc_heap_start = table_end;
  __malloc_heap_end = (RAMEND) - (MAX_STACK_DEPTH);
  /** As the table is outside of the memory area with the normal data,
   * its content will NOT be cleared by the default avr-libc startup
   * code.  So we clear the table memory ourselves.
   */
  memset(table_cur, 0, ((char *)table_end)-((char *)table_cur));
}


/** Print some status messages for debugging */
void ts_print_status(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init8")));
void ts_print_status(void)
{
  uprintf("<ts_print_status>");
  uprintf("table %p", table);
  uprintf("table_cur %p",           table_cur);
  uprintf("table_end %p",           table_end);
  const size_t UV(diff) = table_end - table_cur;
  uprintf("table_end - table_cur 0x%x", _UV(diff));
  const size_t UV(diff2) = ((char*)table_end) - ((char*)table_cur);
  uprintf("table_end - table_cur 0x%x %d %d",
          _UV(diff2), _UV(diff2), _UV(diff2)/sizeof(*table_cur));
  uprintf("</ts_print_status>");
}


/** Initialize peripherals
 *
 * Set up output pins to Pollin Eval Board speaker and LED2.
 */
void io_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void io_init(void)
{
  /** PD7 is Pollin Board speaker */
  /* configure pin 21 as an output                               */
  DDRD |= (_BV(DDD7));
  /* set pin 21 to ground                                        */
  PORTD &= ~_BV(PD7);

  /** PD6 is Pollin Board LED2 */
  /* configure pin 20 as an output                               */
  DDRD |= (_BV(DDD6));
  /* set pin 20 to ground                                        */
  PORTD &= ~_BV(PD6);
}


/** External INT0, i.e. count a GM event */
ISR(INT0_vect)
{

  PORTD ^= _BV(PD6);

  PORTD |= _BV(PD7);
  _delay_us(DELAY_BEEP);
  PORTD &=~ _BV(PD7);
  _delay_us(DELAY_BEEP);
  PORTD |= _BV(PD7);
  _delay_us(DELAY_BEEP);
  PORTD &=~ _BV(PD7);
  _delay_us(DELAY_BEEP);
  PORTD |= _BV(PD7);
  _delay_us(DELAY_BEEP);
  PORTD &=~ _BV(PD7);

  /* without delay (200ns BEEP_DELAY): 59.53  +/- 2.36 CPMs */
  /* _delay_ms(1); 54.94  +/- 2.27 CPMs */
  /* _delay_ms(2); 62.25  +/- 2.42 CPMs (average is within 1 sigma) */

  _delay_ms(2);


  if (table_cur < table_end) {
    histogram_element_inc(table_cur);
  }


  /* debounce any pending ints
     - preller während schaltflanke
     - mehrfachpulse durch alte zählrohre */
  EIFR |= _BV(INTF0);
}


ISR(TIMER1_COMPA_vect)
{
  /* toggle a sign PORTD ^= _BV(PD5); (done automatically) */

  if (!measurement_finished) {
    /** We do not touch the measurement_finished flag ever again after
     * setting it. */
    last_timer_count = timer_count;
    timer_count--;
    if (timer_count == 0) {
      /* Timer has elapsed. Advance to next counter element in time
       * series, and restart the timer countdown. */
      table_cur++;
      if (table_cur < table_end) {
        sizeof_data_table += sizeof(*table_cur);
        timer_count = orig_timer_count;
      } else {
        measurement_finished = 1;
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
void trigger_src_conf(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init7")));
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
  timer_init_quick();
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
