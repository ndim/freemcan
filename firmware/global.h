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
#define BAUDRATE 9600UL

/** ADC clock source frequency in Hz
 *
 * If you put in here a value the prescalers
 * are calculated in order to set the next
 * higher possible ADC frequency. E.g. with
 * F_CPU = 16Mhz and F_ADC_CLK_SRC = 200khz
 * a final ADC frequency of 250khz is set. */
#define F_ADC_CLK_SRC 200000UL

/** ADC resolution in bit
 *
 * Put in there resonable values:
 * E.g. 8 bit resolution at 500 Mhz.
 * (ATMEGA644P has 3,5LSB accuracy at 1Mhz; 4V) */
#define ADC_RESOLUTION 9

/** ADC clock divider */
#define ADC_DIVIDER (F_CPU/F_ADC_CLK_SRC)

/** Timer prescaler settings for 16Bit timer
 *
 *  1: no prescaling
 *  2: divider=8
 *  3: divider=64
 *  4: divider=256
 *  5: divider=1024
 *  Select the prescaler to have an compare match value as integer
 */
#define TIMER_PRESCALER (5)

/** Timer compare match value for 16Bit timer
 *
 *  TIMER_COMPARE_MATCH_VAL = (Time_elpased[sec]*F_CPU[Hz]/Divider) - 1
 *  E.g. (1sec*16000000Hz/1024) - 1 = 15624
 */
#define TIMER_COMPARE_MATCH_VAL 15624

#endif /* !GLOBAL_H */
