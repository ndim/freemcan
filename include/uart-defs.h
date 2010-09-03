/** \file include/uart-defs.h
 * \brief UART communication definitions (layer 1)
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
 * \defgroup uart_defs UART Communication
 * \ingroup communication_protocol
 * @{
 */

#ifndef UART_DEFS_H
#define UART_DEFS_H


/** UART baud rate
 *
 * This value is must be agreed upon by both hostware and firmware.
 *
 * Host side constraints for this value: The well-known set of
 * baudrates the UART supports.
 *
 * Device side constraints for this value: Needs "UL" suffix to make
 * sure the divisions work out correctly for the 16bit integers on the
 * AVR chips. Resulting real baud rate needs to be close enough to the
 * theoretical value so that the host's UART can properly reconstruct
 * the clock signal.
 *
 * \todo Maybe it would be possible for the AVR to store the baud rate
 *       in the EEPROM, and there could be a command to adapt the
 *       firmware's baud rate without needing to recompile and
 *       re-program the firmware?
 */

//#define UART_BAUDRATE 2400UL
//#define UART_BAUDRATE 9600UL
//#define UART_BAUDRATE 38400UL
//#define UART_BAUDRATE 57600UL
#define UART_BAUDRATE 115200UL
//#define UART_BAUDRATE 230400UL
//#define UART_BAUDRATE 460800UL
//#define UART_BAUDRATE 500000UL
//#define UART_BAUDRATE 1000000UL


/** Maximum admissible UART baud rate error 
 *
 * Error between requested baud rate and real baudrate [per mill] 
 * Suggestion is to set this to 1% [10]
 *
 */
 
#define UART_RELTOL 21  // 2.1% Error


/** @} */

#endif /* !UART_DEFS_H */


/*
 * Local Variables:
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
