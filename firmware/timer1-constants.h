/** \file firmware/timer1-constants.h
 * \brief Constants for setting up timer1
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
 * \defgroup timer1_constants Constants for setting up timer1
 * \ingroup firmware_generic
 * @{
 */

#ifndef TIMER1_CONSTANTS_H
#define TIMER1_CONSTANTS_H


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
#if   (F_CPU == 20000000UL)
# ifdef TIMER1_SUB_1SEC
#  define TIMER1_PRESCALER 4
# else
#  define TIMER1_PRESCALER 5
# endif
#elif (F_CPU == 18432000UL)
# ifdef TIMER1_SUB_1SEC
#  define TIMER1_PRESCALER 4
# else
#  define TIMER1_PRESCALER 5
# endif
#elif (F_CPU == 16000000UL)
# ifdef TIMER1_SUB_1SEC
#  define TIMER1_PRESCALER 4
# else
#  define TIMER1_PRESCALER 5
# endif
#else
# error Unsupported F_CPU value
#endif


/** Timer compare match value for 16Bit timer
 *
 *  TIMER1_COMPARE_MATCH_VAL = (time_elpased [sec]*F_CPU [Hz]/Divider) - 1
 *  E.g. (1sec*16000000Hz/1024) - 1 = 15624
 *  E.g. (0.1sec*16000000Hz/256) - 1 = 6249
 *
 *  The data measurement is carried out in multiples of time_elapsed.
 *
 * Lazy people can calculate these constants using Erlang as follows:
 * $ erl
 * 1> Freqs = [ 20000000, 18432000, 16000000 ].
 * [20000000,18432000,16000000]
 * 2> Speeds = [ {0.1, 256}, {1.0, 1024} ].
 * [{0.1,256},{1.0,1024}]
 * 3> [ {Per, F_CPU, Per*F_CPU/Div-1} || F_CPU <- Freqs, {Per,Div} <- Speeds ].
 * [{0.1,20000000,7811.5},
 *  {1.0,20000000,19530.25},
 *  {0.1,18432000,7199.0},
 *  {1.0,18432000,17999.0},
 *  {0.1,16000000,6249.0},
 *  {1.0,16000000,15624.0}]
 * 4> q().
 * ok
 * $
 *
 */
#if   (F_CPU == 20000000UL)
# ifdef TIMER1_SUB_1SEC
#  define TIMER1_COMPARE_MATCH_VAL 7811
# else
#  define TIMER1_COMPARE_MATCH_VAL 19530
#endif
#elif (F_CPU == 18432000UL)
# ifdef TIMER1_SUB_1SEC
#  define TIMER1_COMPARE_MATCH_VAL 7199
# else
#  define TIMER1_COMPARE_MATCH_VAL 17999
# endif
#elif (F_CPU == 16000000UL)
# ifdef TIMER1_SUB_1SEC
#  define TIMER1_COMPARE_MATCH_VAL 6249
# else
#  define TIMER1_COMPARE_MATCH_VAL 15624
# endif
#else
# error Unsupported F_CPU value
#endif


/** Toggle a sign if the measurement is over */
#ifdef TIMER1_SUB_1SEC
# define TIMER1_COMPARE_MATCH_VAL_MEASUREMENT_OVER \
  (((TIMER1_COMPARE_MATCH_VAL)*10UL)>>2)
#endif


/** Timer1 wave form generation mode
 *
 *   0  normal
 *   4  CTC (clear timer on compare match) with OCR1A as top
 *  12  CTC (clear timer on compare match) with ICR1 as top
 *  ...
 *
 */
#define TIMER1_WGM_MODE 4


#endif /* !TIMER1_CONSTANTS_H */


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
