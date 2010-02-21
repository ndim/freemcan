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
#include "global.h"


#define BIT(NO) (1<<(NO))


/** USART0 initialisation to 8 databits no parity
 *
 */
void uart_init(void)
{
  /* These baud setting are valid only for asynchrounous normal mode */
  const uint16_t baud_value=(F_CPU / (16L * BAUDRATE)) - 1;

  UBRR0H=(uint8_t)(baud_value >> 8);
  UBRR0L=(uint8_t)baud_value;

  /* Asynchron (no clk is used); 8 databit with no parity bit (8N1 frame format) */
  UCSR0C = (BIT(UCSZ01) | BIT(UCSZ00));

  /* Enable transmit and receive */
  UCSR0B = (BIT(TXEN0) | BIT(RXEN0));
}


/** Reset checksum accumulator */
static uint16_t checksum_accu;


/** Reset checksum state */
void uart_checksum_reset(void)
{
  checksum_accu = 0x3e59;
}


/** Update checksum
 *
 * \todo Use a good checksum algorithm with good values.
 */
static inline
void uart_checksum_update(const char c)
{
  const uint8_t  n = (uint8_t)c;
  const uint16_t x = 8*n+2*n+n;
  const uint16_t r = (checksum_accu << 3) | (checksum_accu >> 13);
  const uint16_t v = r ^ x;
  checksum_accu = v;
}


/** Send checksum */
void uart_checksum_send(void)
{
  const uint8_t v = checksum_accu & 0xff;
  uart_putc((const char)v);
}


/** Receive a byte and verify whether it matches the checksum
 *
 * \return boolean value in uint8_t
 */
char uart_checksum_recv(void)
{
  const uint8_t v = checksum_accu & 0xff;
  const uint8_t c = uart_getc();
  return (v == c);
}


/** Write character to UART */
void uart_putc(const char c)
{
    /* poll until output buffer is empty */
    loop_until_bit_is_set(UCSR0A, UDRE0);

    /* put the char */
    UDR0 = c;

    /* here would be the place to update the checksum state with c */
    uart_checksum_update(c);
}


/** Write NUL terminated string to UART */
void uart_puts(const char *s)
{
    while (*s)
    {   /* til final string character '\0' */
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

/** Read a character from the UART */
char uart_getc()
{
    /* Poll til a character is inside the input buffer */
    loop_until_bit_is_set( UCSR0A, RXC0 );

    /* Get the character */
    const char ch = UDR0;

    uart_checksum_update(ch);
    return ch;
}
