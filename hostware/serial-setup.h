/** \file hostware/serial-setup.h
 * \brief Serial port access code interface
 *
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
 * \addtogroup serial_setup
 * @{
 */

#ifndef SERIAL_SETUP_H
#define SERIAL_SETUP_H


/** Parity type the serial port is supposed to use */
typedef enum {
  PARITY_NONE
#ifdef PARITY_SUPPORT_NOT_IMPLEMENTED_YET
  PARITY_ODD,
  PARITY_EVEN
#endif
} serial_parity_t;


/** Open serial port device file with the appropriate flags. */
int serial_open(const char *device_name);


/** Set up serial port parameters
 *
 * \param fd device file descriptor
 * \param baudconst A constant from termios.h, e.g. B9600.
 *                  You can use serial_get_baudconst() below to
 *                  determine a good value for baudconst.
 * \param bits_per_byte Bits per transferred byte (7 or 8).
 * \param parity The parity to use (N,E,O). See #serial_parity_t.
 * \param stop_bits Number of stop bits (1 or 2).
 *
 */
void serial_setup(const int fd,
                  const long baudconst,
                  const int bits_per_byte,
                  const serial_parity_t parity,
                  const int stop_bits);


/** Determine baud rate constant from baud rate number.
 *
 * The inverse function of #serial_get_baudrate.
 *
 * \param baudrate An integer number with the baud rate, e.g. 115200.
 * \return Corresponding baud rate constant from termios.h, e.g. B115200.
 */
long serial_get_baudconst(const long baudrate);


/** Determine baud rate number from baud rate constant.
 *
 * The inverse function of #serial_get_baudconst.
 *
 * \param baudconst A baud rate constant from termios.h, e.g. B115200.
 * \return Corresponding integer number with the baud rate, e.g. 115200.
 */
long serial_get_baudrate(const long baudconst);


/** @} */

#endif /* !SERIAL_SETUP_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
