/** \file firmware/global.h
 * \brief Global adjustments for freemcan firmware
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
 * \defgroup global_constants Global Constants And Definitions
 * \ingroup firmware
 * @{
 */

#ifndef GLOBAL_H
#define GLOBAL_H


#ifndef __ASSEMBLER__

#include <stdint.h>


/** XTAL frequency */
#ifndef F_CPU
# error You need to define F_CPU!
#endif


/** Timer prescaler selection (16Bit timer)
 *
 *  1: No prescaling
 *  2: Divider=8
 *  3: Divider=64
 *  4: Divider=256
 *  5: Divider=1024
 *
 *  Select a prescaler to have an compare match value as integer
 */
#if (F_CPU == 18432000UL)
# ifdef TIMER_SUB_1SEC
#  define TIMER_PRESCALER 4
# else
#  define TIMER_PRESCALER 5
# endif
#elif (F_CPU == 16000000UL)
# ifdef TIMER_SUB_1SEC
#  define TIMER_PRESCALER 4
# else
#  define TIMER_PRESCALER 5
# endif
#else
# error Unsupported F_CPU value
#endif


/** Timer compare match value for 16Bit timer
 *
 *  TIMER_COMPARE_MATCH_VAL = (time_elpased [sec]*F_CPU [Hz]/Divider) - 1
 *  E.g. (1sec*16000000Hz/1024) - 1 = 15624
 *  E.g. (0.1sec*16000000Hz/256) - 1 = 6249
 *
 *  The data measurement is carried out in multiples of time_elapsed.
 */
#if (F_CPU == 18432000UL)
# define TIMER_COMPARE_MATCH_VAL 17999
#elif (F_CPU == 16000000UL)
# ifdef TIMER_SUB_1SEC
#  define TIMER_COMPARE_MATCH_VAL 6249
/** Toggle a sign if the measurement is over */
#  define TIMER_COMPARE_MATCH_VAL_MEASUREMENT_OVER (62490 >> 2)
# else
#  define TIMER_COMPARE_MATCH_VAL 15624
# endif
#else
# error Unsupported F_CPU value
#endif




/** @} */

#else /* ifdef __ASSEMBLER__ */

#endif /* __ASSEMBLER__ */

#endif /* !GLOBAL_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
