/** \file uart-comm.c
 * \brief ATmega UART communication implementation
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

#include <avr/io.h>
#include <stdint.h>

#include "registers.h"
#include "uart-comm.h"
#include "clocks.h"


#define BIT(NO) (1<<(NO))


/** USART0 initialisation to 8 databits no parity
 *
 */
void uart_init(void)
{
  /* baud setting valid only  for asynchrounous normal mode */
  const uint16_t baud_value=(F_CPU / (16L * BAUDRATE)) - 1;

  UBRR0H=(uint8_t)(baud_value>>8);
  UBRR0L=(uint8_t)baud_value;

  /* Asynchron (no clk is used); 8 databit no parity (8N1 frame format) */
  UCSR0C = (BIT(UCSZ01) | BIT(UCSZ00));

  /* tx enable */
  UCSR0B = BIT(TXEN0);
}


/** Write character to UART */
void uart_putc(const char c)
{
    /* poll until output buffer is empty */
    loop_until_bit_is_set(UCSR0A, UDRE0);

    /* put the char */
    UDR0 = c;

    /* here would be the place to update the checksum state with c */
}


/** Write NUL terminated string to UART */
void uart_puts(const char *s)
{
    while (*s)
    {   /* til *s != '\0' (not final string character) */
        uart_putc(*s);
        s++;
    }
}


/** Write data buffer of arbitrary size and content to UART */
void uart_putb(const void *buf, size_t len)
{
    const char *s = (const char *)buf;
    while (len > 0)
    {
        uart_putc(*s);
        s++;
	len--;
    }
}
