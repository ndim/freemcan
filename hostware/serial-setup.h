/** \file serial-setup.h
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
 */

#ifndef SERIAL_SETUP_H
#define SERIAL_SETUP_H

/** Open serial port device file with the appropriate flags. */
extern int  serial_open(const char *device_name);

/** Set up serial port parameters */
extern void serial_setup(const int fd, const long baud);

/** Compute which baud rate the user wants.
 *
 * Uses a simple adding hash function.
 */
extern long serial_string_to_baud(const char *arg);

#endif /* !SERIAL_SETUP_H */
