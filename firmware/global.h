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
 * \defgroup global_constants Global Constants And Definitions
 * \ingroup firmware
 * @{
 */

#ifndef GLOBAL_H
#define GLOBAL_H


#ifndef __ASSEMBLER__

#include <stdint.h>


/** XTAL frequency */
#ifndef F_CPU
# error You need to define F_CPU!
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
#if (F_CPU == 18432000UL)
# define TIMER_PRESCALER (5)
#elif (F_CPU == 16000000UL)
# define TIMER_PRESCALER (5)
#else
# error Unsupported F_CPU value
#endif


/** Timer compare match value for 16Bit timer
 *
 *  TIMER_COMPARE_MATCH_VAL = (time_elpased [sec]*F_CPU [Hz]/Divider) - 1
 *  E.g. (1sec*16000000Hz/1024) - 1 = 15624
 *
 *  The data measurement is carried out in multiples of time_elapsed.
 */
#if (F_CPU == 18432000UL)
# define TIMER_COMPARE_MATCH_VAL 17999
#elif (F_CPU == 16000000UL)
# define TIMER_COMPARE_MATCH_VAL 15624
#else
# error Unsupported F_CPU value
#endif


/** Histogram element size in bytes
 *
 * The code should support values of 2, 3, and 4, but we are focussing
 * on 3 from now on.
 */
#define ELEMENT_SIZE_IN_BYTES 3


#if (ELEMENT_SIZE_IN_BYTES == 3)

/** Unsigned 24bit integer type
 *
 * This could be called a uint24_t, but we do not want to intrude on
 * that namespace.
 */
typedef uint8_t freemcan_uint24_t[3];

#endif


/** Histogram element type */
typedef
#if (ELEMENT_SIZE_IN_BYTES == 2)
  uint16_t
#elif (ELEMENT_SIZE_IN_BYTES == 3)
  freemcan_uint24_t
#elif (ELEMENT_SIZE_IN_BYTES == 4)
  uint32_t
#else
# error Unsupported ELEMENT_SIZE_IN_BYTES
#endif
  histogram_element_t;


#if (ELEMENT_SIZE_IN_BYTES == 3)
/** Increment 24bit unsigned integer */
inline static
void histogram_element_zero(volatile freemcan_uint24_t *dest)
{
  asm("\n\t"
      /* store 24 bit value zero */
      "std %a[preg]+2, __zero_reg__\n\t"                    /* 2 cycles */
      "std %a[preg]+1, __zero_reg__\n\t"                    /* 2 cycles */
      "st  %a[preg], __zero_reg__\n\t"                      /* 2 cycles */
      : /* output operands */
      : /* input operands */
        [preg] "b" (dest)
        /* no clobber */
      );
}


inline static
void histogram_element_copy(volatile freemcan_uint24_t *dest,
                            volatile freemcan_uint24_t *source)
{
  asm("\n\t"
      /* load and store 24 bit value in units of 8 bits */
      "ld  __tmp_reg__, %a[src]\n\t"              /* 2 cycles */
      "st  %a[dst], __tmp_reg__\n\t"              /* 2 cycles */
      "ldd __tmp_reg__, %a[src]+1\n\t"            /* 2 cycles */
      "std %a[dst]+1, __tmp_reg__\n\t"            /* 2 cycles */
      "ldd __tmp_reg__, %a[src]+2\n\t"            /* 2 cycles */
      "std %a[dst]+2, __tmp_reg__\n\t"            /* 2 cycles */
      : /* output operands */
      : /* input operands */
        [dst] "b" (dest),
        [src] "b" (source)
        /* no clobbers */
      );
}


inline static
void histogram_element_inc(volatile freemcan_uint24_t *element)
{
  uint16_t accu;
  asm volatile("\n\t"
               /* load 24 bit value */
               "ld  %A[accu],    %a[elem]\n\t"             /* 2 cycles */
               "ldd %B[accu],    %a[elem]+1\n\t"           /* 2 cycles */
               "ldd __tmp_reg__, %a[elem]+2\n\t"           /* 2 cycles */

               /* increment 24 bit value */
               "adiw %[accu], 1\n\t"                       /* 2 cycles for word */
               "adc  __tmp_reg__, __zero_reg__\n\t"        /* 1 cycle */

               /* store 24 bit value */
               "st  %a[elem],   %A[accu]\n\t"              /* 2 cycles */
               "std %a[elem]+1, %B[accu]\n\t"              /* 2 cycles */
               "std %a[elem]+2, __tmp_reg__\n\t"           /* 2 cycles */

               : /* output operands */
                 /* let compiler decide which registers to clobber */
                 [accu] "=&r" (accu)

               : /* input operands */
                 [elem] "b" (element)

                 /* : let compiler decide which regs to clobber via register var accu var */
               );
}
#else
/** Increment 8bit, 16bit, or 32bit unsigned integer */
inline static
void histogram_element_inc(volatile histogram_element_t *element)
{
  (*element)++;
}
#endif


/** @} */

#else /* ifdef __ASSEMBLER__ */

#endif /* __ASSEMBLER__ */

#endif /* !GLOBAL_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
