/** \file global.h
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
 */

#ifndef GLOBAL_H
#define GLOBAL_H

/** XTAL frequency */
#ifndef F_CPU
/* #define F_CPU 1000000UL                     //!< factory configuration: 8Mhz/8 */
#define F_CPU 16000000UL		               //!< Pollin AVR Eval board
#endif

/** UART baud rate */
#define UART_BAUDRATE 9600UL

/** ADC prescaler selection for ADC clock source frequency
 *
 *  0: ADC clock divider=2
 *  1: ADC clock divider=2
 *  2: ADC clock divider=4
 *  3: ADC clock divider=8
 *  4: ADC clock divider=16
 *  5: ADC clock divider=32
 *  6: ADC clock divider=64
 *  7: ADC clock divider=128
 *
 *  Select a prescaler in order to set a resonable ADC clock frequency.
 *  For ATMEGA644P the nominal frequency range lies between 50 - 200kHz.
 *
 *  ADC clock division factor = F_CPU [Hz]/ADC clock source frequency [Hz]
 *  E.g. 16000kHz/64 = 250khz
 */
#define ADC_PRESCALER (6)

/** ADC resolution in bit
 *
 *  Put in here resonable value:
 *  E.g. 8 bit resolution at 500 kHz.
 *  (ATMEGA644P has 3,5LSB accuracy at 1Mhz; 4V)
 */
#define ADC_RESOLUTION (9)

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
#define TIMER_PRESCALER (5)

/** Timer compare match value for 16Bit timer
 *
 *  TIMER_COMPARE_MATCH_VAL = (time_elpased [sec]*F_CPU [Hz]/Divider) - 1
 *  E.g. (1sec*16000000Hz/1024) - 1 = 15624
 *
 *  The data measurement is carried out in multiples of time_elapsed.
 */
#define TIMER_COMPARE_MATCH_VAL 15624

#endif /* !GLOBAL_H */
