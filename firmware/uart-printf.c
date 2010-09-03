/** \file firmware/uart-printf.c
 * \brief Implement printf via UART
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
 * \defgroup uart_printf printf() like functionality via frames over UART
 * \ingroup firmware
 *
 * Inclusion into the firmware image is optional.
 *
 * @{
 */
#include <avr/io.h>
#include <stdarg.h>
#include <stdio.h>

#include "global.h"

#include "uart-printf.h"
#include "uart-comm.h"
#include "frame-comm.h"
#include "frame-defs.h"


/** The buffer we format our message into. */
static char buf[256];


/** Our own printf method
 *
 * We want to send all data as packets, so we need to be able to send
 * a header before the actual formatted string, and possibly a tail
 * after it.
 *
 * The avr-libc interface to FILE streams only allows per-character
 * hook calls; there are no per-printf-call hooks. That requires us to
 * implement our own printf() like function here.
 */
void uprintf(const char *format, ...)
{
  va_list vl;
  va_start(vl, format);
  int strsize = vsnprintf(buf, sizeof(buf), format, vl);
  frame_send(FRAME_TYPE_TEXT, buf, strsize);
  va_end(vl);
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
