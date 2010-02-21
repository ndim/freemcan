/** \file registers.h
 * \brief Register use definition for freemcan ATmega firmware
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

#ifndef REGISTERS_H
#define REGISTERS_H

/* Safe registers to reserve for special purposes are r2..r7, apparently:
 * http://www.nongnu.org/avr-libc/user-manual/FAQ.html#faq_regbind
 */

#ifdef __ASSEMBLER__

/* Define the same special use registers as the C compiler uses below */
# define sreg_save r7

#else

#include <stdint.h>

/** Reserve register for special use by assembly language ISR.
 *
 * The C compiler will not touch this register then! */
register uint8_t sreg_save asm("r7");

#endif /* !__ASSEMBLER__ */

#endif /* !REGISTERS_H */
