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

#include <avr/pgmspace.h>

#include <stdint.h>
#include <stdlib.h>

#include "checksum.h"


void uart_putc(const char c);
void uart_putb(const void *buf, size_t len);
void uart_putb_P(PGM_VOID_P buf, size_t len);


/** Read a character from the UART */
inline static
char uart_getc(void)
{
    /* Poll til a character is inside the input buffer */
    loop_until_bit_is_set(UCSR0A, RXC0);

    /* Get the character */
    const char ch = UDR0;

    return ch;
}


extern checksum_accu_t uart_cs_accu_send;


void uart_send_checksum(void);


extern checksum_accu_t uart_cs_accu_recv;


/** Reset the receive checksum state */
inline static
void uart_send_checksum_reset(void)
{
  uart_cs_accu_send = checksum_reset();
}


/** Reset the receive checksum state */
inline static
void uart_recv_checksum_reset(void)
{
  uart_cs_accu_recv = checksum_reset();
}


/** Whether received byte sequence has ended with a valid checksum.
 *
 * \return boolean value within a char
 */
inline static
char uart_recv_checksum_matches(void)
{
  return checksum_matches(uart_cs_accu_recv);
}


void uart_recv_checksum_update(const char ch);


/** @} */


#endif /* !UART_COMM_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
