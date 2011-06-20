/** \file firmware/beep.c
 * \brief Providing a "beep" loudspeaker function
 *
 * \author Copyright (C) 2011 samplemaker
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
 * \defgroup beep loudspeaker function
 * \ingroup firmware_generic
 *
 * Providing a "beep" loudspeaker function
 *
 * @{
 */

#include <avr/io.h>
#include <avr/interrupt.h>

#include "global.h"
#include "beep.h"


/** Configure used pins */
void beep_io_init_used_pins(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void beep_io_init_used_pins(void)
{
  /* configure pin 21 as an output                               */
  DDRD |= _BV(DDD7);
  /* set pin 21 to ground                                        */
  PORTD &= ~_BV(PD7);
}


/** Set up timer2 for wave generator and timer0 for gating signal
 */
void beep_init_and_start(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init7")));
void beep_init_and_start(void)
{
  /** configure square wave generator (timer2)
   */
  /** reset current timer counter value */
  TCNT2  = 0;
  /** output compare A value */
  OCR2A  = TIMER2_COMPA_VALUE;
  /** we do not use timer2's asynchronous mode */
  ASSR   = 0;
  /** set up timer2 mode, and start it with appropriate prescale value */
  TCCR2A = (BITF(TIMER2_WGM_VALUE, WGM2, 0) |
            BITF(TIMER2_WGM_VALUE, WGM2, 1));
  TCCR2B = BITF(TIMER2_WGM_VALUE, WGM2, 2);
  /** map square wave generator (timer2) to output pin PD7 (21) (toggle pin on CTC) */
  TCCR2A |= _BV(COM2A0);

  /** configure gating signal (timer0)
   */
  /** reset current timer counter value */
  TCNT0  = TIMER0_INIT_VALUE;
  /** set up timer0 mode, and start it with appropriate prescale value */
  TCCR0A = (BITF(TIMER0_WGM_VALUE, WGM0, 0) |
            BITF(TIMER0_WGM_VALUE, WGM0, 1));
  TCCR0B = BITF(TIMER0_WGM_VALUE, WGM0, 2);
  /** OVF interrupt enable on timer0 */
  TIMSK0 = _BV(TOIE0);
}


inline static
void timer0_2_stop_and_reset(void)
{
  /* stop gating signal (timer0) */
  TCCR0B &=~ (BITF(TIMER0_CLOCK_PRESCALER_SELECT,  CS0,  2) |
              BITF(TIMER0_CLOCK_PRESCALER_SELECT,  CS0,  1) |
              BITF(TIMER0_CLOCK_PRESCALER_SELECT,  CS0,  0));
  /* reset timer value */
  TCNT0  = TIMER0_INIT_VALUE;

  /* switch off square wave generator (timer2) */
  TCCR2B &=~ (BITF(TIMER2_CLOCK_PRESCALER_SELECT,  CS2,  2) |
              BITF(TIMER2_CLOCK_PRESCALER_SELECT,  CS2,  1) |
              BITF(TIMER2_CLOCK_PRESCALER_SELECT,  CS2,  0));
}


/** Gating signal elapsed (timer0)
 *
 *  Stop and reset timer0 & 2
 */
ISR(TIMER0_OVF_vect)
{
  timer0_2_stop_and_reset();
}


/** Kill a running beep
 *
 * If cli() is called during a running beep the beep needs to be killed
 * explicitely otherwise the beep would keep on running for a infinite time
 */
void beep_kill_all(void)
{
  timer0_2_stop_and_reset();
  /* clear pending interrupt flag */
  TIFR0 |= _BV(TOV0);
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
