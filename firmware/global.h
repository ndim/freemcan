/** \file firmware/global.h
 * \brief Global adjustments for freemcan firmware
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
 * \note global.h MUST be included by ALL firmware *.c files, due to
 *       its globally reserving registers for certain global
 *       variables.
 *
 * \defgroup global_constants Global Constants
 * \ingroup firmware
 * @{
 */

#ifndef GLOBAL_H
#define GLOBAL_H


#ifndef __ASSEMBLER__

#include <stdint.h>

/** XTAL frequency */
#ifndef F_CPU
/* #define F_CPU 1000000UL                     //!< factory configuration: 8Mhz/8 */
#define F_CPU 16000000UL                               //!< Pollin AVR Eval board
#endif

/** Timer prescaler selection (16Bit timer)
 *
 *  1: No prescaling
 *  2: Divider=8
 *  3: Divider=64
 *  4: Divider=256
 *  5: Divider=1024
 *
 *  Select a prescaler to have an compare match value as integer
 */
#define TIMER_PRESCALER (5)

/** Timer compare match value for 16Bit timer
 *
 *  TIMER_COMPARE_MATCH_VAL = (time_elpased [sec]*F_CPU [Hz]/Divider) - 1
 *  E.g. (1sec*16000000Hz/1024) - 1 = 15624
 *
 *  The data measurement is carried out in multiples of time_elapsed.
 */
#define TIMER_COMPARE_MATCH_VAL 15624


/** Histogram element size in bytes
 *
 * The code should support values of 2, 3, and 4, but we are focussing
 * on 3 from now on.
 */
#define ELEMENT_SIZE_IN_BYTES 2


#if (ELEMENT_SIZE_IN_BYTES == 2)

typedef uint16_t histogram_element_t;

#elif (ELEMENT_SIZE_IN_BYTES == 3)

/* This could be called a uint24_t... but we do not want to intrude on
 * that namespace. */
typedef uint8_t freemcan_uint24_t[3];
typedef freemcan_uint24_t histogram_element_t;

#elif (ELEMENT_SIZE_IN_BYTES == 4)

typedef uint32_t histogram_element_t;

#else
# error Unsupported ELEMENT_SIZE_IN_BYTES
#endif


#if (ELEMENT_SIZE_IN_BYTES == 3)
inline static
void histogram_element_inc(volatile freemcan_uint24_t *element)
{
  asm("\n\t"
      /* load 24 bit value */
      "ld  r24, Z\n\t"                      /* 2 cycles */
      "ldd r25, Z+1\n\t"                    /* 2 cycles */
      "ldd __tmp_reg__, Z+2\n\t"            /* 2 cycles */

      /* increase 24 bit value by one */
      "adiw r24, 1\n\t"                     /* 2 cycles for word (r25:r24) */
      "adc  __tmp_reg__, __zero_reg__\n\t"  /* 1 cycle */

      /* store 24 bit value */
      "std Z+2, __tmp_reg__\n\t"            /* 2 cycles */
      "std Z+1, r25\n\t"                    /* 2 cycles */
      "st  Z, r24\n\t"                      /* 2 cycles */
      : /* output operands */
      : /* input operands */
        "z" (element)
      : "r24", "r25"
      );
}
#else
inline static
void histogram_element_inc(volatile histogram_element_t *element)
{
  (*element)++;
}
#endif


/** @} */

/* reserve a few registers for ISR */
register uint8_t sreg_save asm("r7");

#else /* ifdef __ASSEMBLER__ */

# define sreg_save r7

#endif /* __ASSEMBLER__ */

#endif /* !GLOBAL_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
