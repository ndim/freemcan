/** \file clocks.h
 * \brief ATmega clock related constant definitions
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

#ifndef CLOCKS_H
#define CLOCKS_H

#ifndef F_CPU
/* #define F_CPU 1000000UL                     //!< werksauslieferung 8mhz/8 */
#define F_CPU 16000000UL		//!< Pollin AVR Eval board
#endif

/** UART baud rate */
#define BAUDRATE 9600UL

/** ADC clock rate */
#define F_ADC_CLK_SRC 200000UL                // hz

/** ADC clock division factor */
#define ADC_DIVISION_FACTOR (F_CPU/F_ADC_CLK_SRC)

#endif /* !CLOCKS_H */
