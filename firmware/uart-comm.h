/** \file firmware/uart-comm.h
 * \brief ATmega UART communication interface (layer 1)
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
 * \addtogroup uart_comm
 * @{
 */

#ifndef UART_COMM_H
#define UART_COMM_H

#include <stdlib.h>

void uart_init(void);
void uart_putc(const char c);
void uart_putb(const void *buf, size_t len);
char uart_getc(void);

void uart_checksum_reset(void);
void uart_checksum_send(void);
char uart_checksum_recv(void);

/** @} */

#endif /* !UART_COMM_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
