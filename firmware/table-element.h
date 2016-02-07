/** \file firmware/table-element.h
 * \brief Table Element type definitions
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
 * \defgroup table_element Table Element type
 * \ingroup firmware_generic
 * @{
 */

#ifndef TABLE_ELEMENT_H
#define TABLE_ELEMENT_H


#ifndef BITS_PER_VALUE
# error Error: You MUST define BITS_PER_VALUE before #include "table-element.h"!
#endif


#if (BITS_PER_VALUE == 8)

/** Histogram element type */
typedef uint8_t  table_element_t;

#elif (BITS_PER_VALUE == 16)

/** Histogram element type */
typedef uint16_t  table_element_t;

#elif (BITS_PER_VALUE == 24)

# ifdef USE_GCC_UINT24

/** Histogram element type */
typedef __uint24  table_element_t;

# else

/** Unsigned 24bit integer type
 *
 * This could be called a uint24_t, but we do not want to intrude on
 * that namespace.
 */
typedef uint8_t freemcan_uint24_t[3];

/** Histogram element type */
typedef freemcan_uint24_t table_element_t;

# endif

#elif (BITS_PER_VALUE == 32)

/** Histogram element type */
typedef uint32_t table_element_t;

#else
# error Unsupported BITS_PER_VALUE value
#endif


#if (BITS_PER_VALUE == 24) && (!defined(USE_GCC_UINT24))
/** Set 24bit unsigned integer to zero */
inline static
void table_element_zero(volatile freemcan_uint24_t *dest)
{
  asm volatile("\n\t"
               /* store 24 bit value zero */
               "std	%a[preg]+2, __zero_reg__\n\t"     /* 2 cycles */
               "std	%a[preg]+1, __zero_reg__\n\t"     /* 2 cycles */
               "st	%a[preg],   __zero_reg__\n\t"     /* 2 cycles */
               : /* output operands */
               : /* input operands */
                 [preg] "b" (dest)
                 /* no clobber */
               );
}


/** Copy 24bit unsigned integer */
inline static
void table_element_copy(volatile freemcan_uint24_t *dest,
                        volatile freemcan_uint24_t *source)
{
  asm volatile("\n\t"
               /* load and store 24 bit value in units of 8 bits */
               "ld	__tmp_reg__, %a[src]\n\t"         /* 2 cycles */
               "st	%a[dst],     __tmp_reg__\n\t"     /* 2 cycles */
               "ldd	__tmp_reg__, %a[src]+1\n\t"       /* 2 cycles */
               "std	%a[dst]+1,   __tmp_reg__\n\t"     /* 2 cycles */
               "ldd	__tmp_reg__, %a[src]+2\n\t"       /* 2 cycles */
               "std	%a[dst]+2,   __tmp_reg__\n\t"     /* 2 cycles */
               : /* output operands */
               : /* input operands */
                 [dst] "b" (dest),
                 [src] "b" (source)
                 /* no clobbers */
               );
}


/** Increment 24bit unsigned integer */
inline static
void table_element_inc(volatile freemcan_uint24_t *element)
{
  uint16_t accu;
  asm volatile("\n\t"
               /* load 24 bit value */
               "ld	%A[accu],    %a[elem]\n\t"        /* 2 cycles */
               "ldd	%B[accu],    %a[elem]+1\n\t"      /* 2 cycles */
               "ldd	__tmp_reg__, %a[elem]+2\n\t"      /* 2 cycles */

               /* increment 24 bit value */
               "adiw	%[accu],     1\n\t"               /* 2 cycles */
               "adc	__tmp_reg__, __zero_reg__\n\t"    /* 1 cycle  */

               /* store 24 bit value */
               "st	%a[elem],    %A[accu]\n\t"        /* 2 cycles */
               "std	%a[elem]+1,  %B[accu]\n\t"        /* 2 cycles */
               "std	%a[elem]+2,  __tmp_reg__\n\t"     /* 2 cycles */

               : /* output operands */
                 /* Let compiler decide which registers to
                  * clobber. However, the adiw instruction only
                  * works on r24:r25, r26:r27, r28:r29, r30:r31, so
                  * &w is the appropriate letter for the clobber
                  * accu.
                  */
                 [accu] "=&w" (accu)

               : /* input operands */
                 [elem] "b" (element)

                 /* : let compiler decide which regs to clobber via register var accu var */
               );
}


/** Compare table element to value for equality.
 *
 * \param element Pointer to the table element to compare.
 * \param value   The value to compare the element against.
 *
 * \return Boolean value for equality in an uint8_t
 *
 * Note that #value has a different type from #*element here, as the
 * compiler would not know how to generate or pass such a
 * freemcan_uint24_t value.
 *
 * Caveats: Ignores the upper 8bit of the uint32_t constant. Generates
 * more instructions than strictly necessary. Clobbers more registers
 * than strictly necessary. Generates a lot more instructions than
 * strictly necessary if compiling to an immediate value.
 */
inline static
uint8_t table_element_cmp_eq(volatile freemcan_uint24_t *element,
                             const uint32_t value)
{
  uint8_t result_bool;
  asm volatile("\n\t"
               /* presume non-equal, i.e. false */
               "ldi	%[result],    0\n\t"           /* 1 cycle  */

               "ldd	__tmp_reg__,  %a[elem]+2\n\t"  /* 2 cycles */
               "cp	__tmp_reg__,  %C[valu]\n\t"    /* 1 cycle  */

               "ldd	__tmp_reg__,  %a[elem]+1\n\t"  /* 2 cycles */
               "cpc	__tmp_reg__,  %B[valu]\n\t"    /* 1 cycle  */

               "ld	__tmp_reg__,  %a[elem]\n\t"    /* 2 cycles */
               "cpc	__tmp_reg__,  %A[valu]\n\t"    /* 1 cycle  */

               "brne	L%=_end\n\t"                /* 1/2 cycles  */
               "ldi	%[result],    1\n\t"           /* 1 cycle  */
               "\n"
               "L%=_end:\n"

               : /* output operands */
                 [result] "=&r" (result_bool)
               : /* input operands */
                 [elem] "b" (element),
                 [valu] "r" (value)
               );
  return result_bool;
}


#else

/** Compiler code to zero 8bit, 16bit, 24bit, or 32bit unsigned integer */
inline static
void table_element_zero(volatile table_element_t *dest)
{
  *dest = 0;
}

/** Compiler code to copy 8bit, 16bit, 24bit, or 32bit unsigned integer */
inline static
void table_element_copy(volatile table_element_t *dest,
                        volatile table_element_t *source)
{
  *dest = *source;
}

/** Compiler code to increment 8bit, 16bit, 24bit, or 32bit unsigned integer */
inline static
void table_element_inc(volatile table_element_t *element)
{
  (*element)++;
}

/** Compiler code to compare 8bit, 16bit, 24bit or 32bit unsigned
 *  integer to a value for equality.
 *
 * \param element Pointer to the table element
 * \param value   Value to compare table element with.
 * \return Boolean value for equality in an uint8_t
 *
 * Note that #value has the same type as #*element here.
 */
inline static
uint8_t table_element_cmp_eq(volatile table_element_t *element,
                             const table_element_t value)
{
  return ((*element) == value);
}

#endif


#endif /* !TABLE_ELEMENT_H */


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
