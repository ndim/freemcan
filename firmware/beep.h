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
 *  Gating signal (timer0)
 */

/** Used for gating the wave generator for the beep signal */
#define TIMER0_CLOCK_DIVISION_FACTOR 1024ULL

/** Timer prescaler values timer0 for writing to the CS02:CS00 bits */
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

/** Timer0 is running in dual slope operation.
  * Starting from TIMER0_INIT_VALUE the counter is incremented
  * until the counter value matches 0xFF. When the counter reaches
  * 0xFF, it changes the count direction. The TCNT0 value will be equal
  * to 0xFF for one timer clock cycle. Reaching 0x00
  * an overflow interrupt is executed. The timer is executing N total
  * counts where:
  * N = 2*0xFF-TIMER0_INIT_VALUE
  * The gating time is
  * T = N*TIMER0_CLOCK_DIVISION_FACTOR/F_CPU
  *
  * Possible lengths of gating signal (beep lengths) [ms]
  * 14.2ms .. 28.2ms @ 1024 for 18.432Mhz uC
  * 3.6ms  .. 7.0ms  @ 256  for 18.432Mhz uC
  */
#define BEEP_LENGTH 21

/** Calculate timer start value on timer0 */
#define TIMER0_INIT_VALUE                              \
   (0x1FEULL - (BEEP_LENGTH) * ((F_CPU)/1000ULL) /     \
   (TIMER0_CLOCK_DIVISION_FACTOR))                     \

/* TCTN0 = 0 not allowed; results in immediate OVF */
#if (TIMER0_INIT_VALUE < 1)
# error TIMER0_INIT_VALUE: Too low => try to increase timer0 divider
#endif

#if (TIMER0_INIT_VALUE > 254)
# error TIMER0_INIT_VALUE: Too high => try to decrease timer0 divider
#endif

/** Run timer0 in dual slope mode to realize longer beeplengths */
#define TIMER0_WGM_VALUE 1


/**
* Square wave generator waveform generation for beep signal (timer2)
*/

/** Timer prescaler value for frequency calculations */
#define TIMER2_CLOCK_DIVISION_FACTOR 128ULL

/** Timer prescaler values timer2 for writing to the CS22:CS20 bits */
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

/** Frequency timer0 [Hz] (resonant frequency of piezo sounder) */
#define BEEP_FREQUENCY 4000ULL

/** Gather for nearest possible compare match value on timer2 */
#define TIMER2_COMPA_VALUE                                        \
  (((F_CPU) /                                                     \
   (0x2ULL * (BEEP_FREQUENCY) * (TIMER2_CLOCK_DIVISION_FACTOR)) ) \
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


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
