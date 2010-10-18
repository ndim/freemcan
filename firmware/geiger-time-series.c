/** \file firmware/geiger-time-series.c
 * \brief Geiger Counter recording time series
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


/** The table
 *
 * Note that we have the table location and size determined by the
 * linker script time-series-table.x.
 */
extern volatile table_element_t data_table[];

/** End of the table: Still needs rounding */
extern volatile table_element_t data_table_end[];


/** End of the table: Never write to *table_cur when (table_cur>=table_end)! */
volatile table_element_t *volatile table_end =
  (table_element_t volatile *)((char *)data_table_end -
                                   (sizeof(table_element_t)-1));

/** Pointer to the current place to store the next value at */
volatile table_element_t *volatile table_cur = data_table;


/** Actual size of #data_table in bytes
 *
 * We update this value whenever new time series data has been
 * recorded. The initial value is "one element".
 *
 * \see data_table
 *
 * Note: We put this value into the .data.sizeof section in order to
 *       easily automatically extract the data table element size
 *       during the later build stages.
 */
size_t sizeof_data_table __attribute__ (( section(".data.sizeof") )) =
  sizeof(data_table[0]);


/** Setup, needs to be called once on startup */
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


/** Print some status messages for debugging */
void ts_print_status(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init8")));
void ts_print_status(void)
{
#ifdef VERBOSE_STARTUP_MESSAGES
  uprintf("<ts_print_status>");
  uprintf("%-25s %p", "data_table", data_table);
  uprintf("%-25s %p", "table_cur",  table_cur);
  uprintf("%-25s %p", "table_end",  table_end);
  const size_t UV(sizeof_table) = ((char*)table_end) - ((char*)table_cur);
  uprintf("%-25s 0x%x = %d >= %d * %d",
          "table_end - table_cur",
          _UV(sizeof_table), _UV(sizeof_table),
          _UV(sizeof_table)/sizeof(*table_cur), sizeof(*table_cur));
  uprintf("</ts_print_status>");
#else
  /** The "#### ##" string is just a placeholder. There is a
   *  GNUmakefile rule to replace that placeholder with a string just
   *  after linking. The linking would gives the necessary information
   *  to fill in.
   */
  send_text_P(PSTR("geiger-time-series: data table of #### elements of ##bit each"));
#endif
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
    table_element_inc(table_cur);
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
