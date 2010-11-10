/** \file firmware/uart-comm.c
 * \brief ATmega UART communication implementation (layer 1)
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
 * \defgroup uart_comm Firmware UART Communications
 * \ingroup firmware_generic
 *
 * Implements the byte stream part of the communication protocol
 * (Layer 1).
 *
 * @{
 */

#include <avr/io.h>
#include <stdint.h>

#include "uart-comm.h"
#include "global.h"

#include "uart-defs.h"
#include "checksum.h"


/* We want to avoid switching to double speed mode as long as
 * possible, as in normal speed mode the receiver will sample more
 * often than in double speed mode.
 *
 * On the other hand, the relative baud rate error (depending
 * on F_CPU) might turn out to be less in double speed mode.
 *
 * So double speed works at 115200 with F_CPU = 16MHz on my system
 * but not single speed. -ndim
 *
 * (-> 2.1% error) may work but in general an error < 1% 
 * is suggested -samplemaker
 */

/* Check settings in asynchronous normal speed mode */
#define UBRR_VALUE ((F_CPU+UART_BAUDRATE*8UL)/(UART_BAUDRATE*16UL)-1UL)
#define BAUD_REAL (F_CPU/(16UL*(UBRR_VALUE+1UL)))
#define BAUD_ERROR ((BAUD_REAL*1000UL)/UART_BAUDRATE)

/* If baud error is too high revert to double speed mode */
#if ((BAUD_ERROR<(1000-UART_RELTOL)) || (BAUD_ERROR>(1000+UART_RELTOL)))
  #define USE_2X 1
  /* #warning baud error too high: reverting to asynchronous double speed mode */
#else
  #define USE_2X 0
#endif

/* Check asynchronous double speed mode if requested */
#if USE_2X
#undef UBRR_VALUE
#undef BAUD_REAL
#undef BAUD_ERROR
#define UBRR_VALUE ((F_CPU+UART_BAUDRATE*4UL)/(UART_BAUDRATE*8UL)-1UL)
#define BAUD_REAL (F_CPU/(8UL*(UBRR_VALUE+1UL)))
#define BAUD_ERROR ((BAUD_REAL*1000UL)/UART_BAUDRATE)

/* If nothing helps give up */
#if ((BAUD_ERROR<(1000-UART_RELTOL)) || (BAUD_ERROR>(1000+UART_RELTOL)))
  #error baud error is too high also in UART asynchronous double speed mode!
#endif

#endif

#define UBRRL_VALUE (UBRR_VALUE & 0xff)
#define UBRRH_VALUE (UBRR_VALUE >> 8)


static checksum_accu_t cs_accu_send;
static checksum_accu_t cs_accu_recv;


/** USART0 initialisation to 8 databits no parity
 *
 */
void uart_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void uart_init(void)
{
  /* set baud rate */

  UBRR0H=UBRRH_VALUE;
  UBRR0L=UBRRL_VALUE;

  /* Asynchronous (no clock is used); 8 databit with no parity bit (8N1
   * frame format) */
  UCSR0C = (_BV(UCSZ01) | _BV(UCSZ00));

  /* Enable transmit and receive */
  UCSR0B = (_BV(TXEN0) | _BV(RXEN0));

  /* Clear or set U2X0 baudrate doubling bit, depending on
   * UART_BAUDRATE. Also disable multi device mode, and do not clear
   * the TXC0 bit (you would *clear* TXC0 bit by writing a 1). */
  UCSR0A = (USE_2X<<U2X0);

  cs_accu_send = checksum_reset();
  cs_accu_recv = checksum_reset();
}


/** Send checksum */
void uart_send_checksum(void)
{
  const uint8_t v = cs_accu_send & 0xff;
  uart_putc((const char)v);
}


void uart_send_checksum_reset(void)
{
  cs_accu_send = checksum_reset();
}


/** Write character to UART */
void uart_putc(const char c)
{
    /* poll until output buffer is empty */
    loop_until_bit_is_set(UCSR0A, UDRE0);

    /* put the char */
    UDR0 = c;

    /* update the checksum state with c */
    cs_accu_send = checksum_update(cs_accu_send, c);
}


/** Write data buffer of arbitrary size and content to UART */
void uart_putb(const void *buf, size_t len)
{
  for (const char *s = (const char *)buf; len > 0; s++, len--) {
    uart_putc(*s);
  }
}


void uart_putb_P(PGM_VOID_P buf, size_t len)
{
  for (PGM_P s = buf; len > 0; s++, len--) {
    uart_putc(pgm_read_byte(s));
  }
}


/** Read a character from the UART */
char uart_getc()
{
    /* Poll til a character is inside the input buffer */
    loop_until_bit_is_set( UCSR0A, RXC0 );

    /* Get the character */
    const char ch = UDR0;

    return ch;
}




/** Check whether received byte c and matches the checksum
 *
 * \return boolean value in char
 */
char uart_recv_checksum_matches(const uint8_t data)
{
  return checksum_matches(cs_accu_recv, data);
}


void uart_recv_checksum_reset(void)
{
  cs_accu_recv = checksum_reset();
}


/* update the checksum state with ch */
void uart_recv_checksum_update(const char ch)
{
  cs_accu_recv = checksum_update(cs_accu_recv, ch);
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
