/** \file firmware/beep.h
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
 * \addtogroup beep loudspeaker function
 * @{
 */


#ifndef BEEP_H
#define BEEP_H


/**
 *  Gating signal (Timer0)
 */

/** Used for gating the wave generator for the beep signal */
#define TIMER0_CLOCK_DIVISION_FACTOR 1024ULL

/* Note: The beep signal length in dual slope mode is:
   ie 509*Divider/FCPU = 509*1024/18432000Hz = 28ms
*/

/** Timer prescaler values TIMER2 for writing to the CS02:CS00 bits
 */
#if   (TIMER0_CLOCK_DIVISION_FACTOR == 1024ULL)
# define TIMER0_CLOCK_PRESCALER_SELECT 5
#elif (TIMER0_CLOCK_DIVISION_FACTOR == 256ULL)
# define TIMER0_CLOCK_PRESCALER_SELECT 4
#elif (TIMER0_CLOCK_DIVISION_FACTOR == 64ULL)
# define TIMER0_CLOCK_PRESCALER_SELECT 3
#elif (TIMER0_CLOCK_DIVISION_FACTOR == 8ULL)
# define TIMER0_CLOCK_PRESCALER_SELECT 2
#elif (TIMER0_CLOCK_DIVISION_FACTOR == 1ULL)
# define TIMER0_CLOCK_PRESCALER_SELECT 1
#else
# error Invalid TIMER0_CLOCK_DIVISION_FACTOR value!
#endif

/** Run Timer0 in dual slope mode (1 or 5) */
#define TIMER0_WGM_VALUE 1

/**
* Square wave generator waveform generation for beep signal (Timer2)
*/

/** Timer prescaler value for frequency calculations */
#define TIMER2_CLOCK_DIVISION_FACTOR 128ULL

/** Timer prescaler values TIMER2 for writing to the CS02:CS00 bits
 */
#if   (TIMER2_CLOCK_DIVISION_FACTOR == 1024ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 7
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 256ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 6
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 128ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 5
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 64ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 4
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 32ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 3
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 8ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 2
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 1ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 1
#else
# error Invalid TIMER2_CLOCK_DIVISION_FACTOR value!
#endif

/** Frequency Timer0 [Hz]*/
#define BEEP_FREQUENCY 7000ULL

/** Gather for nearest possible compare match value on Timer2 */
#define TIMER2_COMPA_VALUE                               \
  (((F_CPU) /                                            \
   ((BEEP_FREQUENCY) * (TIMER2_CLOCK_DIVISION_FACTOR)) ) \
     -1)

#if (TIMER2_COMPA_VALUE < 10)
# error TIMER2_COMPA_VALUE: timer2 accuracy too low => try to decrease timer0 divider
#endif

#if (TIMER2_COMPA_VALUE > 255)
# error TIMER2_COMPA_VALUE: value exceeds range => try to increase timer0 divider
#endif

/** Run timer2 in CTC mode */
#define TIMER2_WGM_VALUE 2


void beep_kill_all(void);


inline static
void _beep(void)
{
    /* start sqare wave generator */
    TCCR2B |= (BITF(TIMER2_CLOCK_PRESCALER_SELECT,  CS2,  2) |
               BITF(TIMER2_CLOCK_PRESCALER_SELECT,  CS2,  1) |
               BITF(TIMER2_CLOCK_PRESCALER_SELECT,  CS2,  0));

    /* trigger gating signal (start timer0) */
    TCCR0B |= (BITF(TIMER0_CLOCK_PRESCALER_SELECT,  CS0,  2) |
               BITF(TIMER0_CLOCK_PRESCALER_SELECT,  CS0,  1) |
               BITF(TIMER0_CLOCK_PRESCALER_SELECT,  CS0,  0));
}

#endif /* !BEEP_H */


/** @} */

