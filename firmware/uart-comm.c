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
 * \ingroup firmware
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
  #warning baud error too high: reverting to asynchronous double speed mode
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


/** USART0 initialisation to 8 databits no parity
 *
 */
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
}


/** Checksum accumulator */
static uint16_t checksum_accu;


/** Reset checksum state */
void uart_checksum_reset(void)
{
  checksum_accu = 0x3e59;
}


/** Update checksum
 *
 * \todo Use a good checksum algorithm with good values.
 *
 * We are calling this function twice - so not inlining the code saves
 * us some bytes that need to be programmed into the uC. For some
 * reason, gcc inlines the code anyway.
 */
static
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


/** Write data buffer of arbitrary size and content to UART */
void uart_putb(const void *buf, size_t len)
{
  for (const char *s = (const char *)buf; len > 0; s++, len--) {
    uart_putc(*s);
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

/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
