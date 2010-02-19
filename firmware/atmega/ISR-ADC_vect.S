/** \file ISR-ADC_vect.S
 * \brief Assembly language implementation for ISR(ADC_vect)
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


#include <avr/io.h>
#include "registers.h"

#if defined(__AVR_ATmega644__) || defined(__AVR_ATmega644P__)
#else
# error Unsupported MCU!
#endif

/* define register names in a single place */
#define tmpcnt0 r24
#define tmpcnt1 r25
#define tmpcnt2 r26
#define tmpcnt3 r27

#define tmpidx0 r30
#define tmpidx1 r31

#define tmpidxW Z

#define HISTOGRAM_DEPTH_32BIT

/* avr-gcc docs say that __zero_reg__ is always r1 */
#define __zero_reg__ r1

.extern	table
.extern	measurement_count

.global my_ADC_vect
my_ADC_vect:	/* interrupt entry costs another 5 cycles */		/* 5 */

	/* save SREG */
	in	sreg_save, _SFR_IO_ADDR(SREG)				/* 1 */

	/* save pointer register */
	push	tmpidx0							/* 2 */
	push	tmpidx1							/* 2 */

	/* save temp counter */
	push	tmpcnt0							/* 2 */
	push	tmpcnt1							/* 2 */
	push	tmpcnt2							/* 2 */
#ifdef HISTOGRAM_DEPTH_32BIT
	push	tmpcnt3							/* 2 */
#endif

	/* Read 8bit value from ADC, extend to 16bit value */
	lds	tmpidx0, ADCH	// Read highest 8 bits of ADC value a	/* 2 */
	clr	tmpidx1		// Throw away the lowest 2 bits		/* 1 */

	/* multiply a by 4 to get table offset */
	add	tmpidx0, tmpidx0	// (2*a)			/* 1 */
	adc	tmpidx1, tmpidx1					/* 1 */
	add	tmpidx0, tmpidx0	// (4*a)			/* 1 */
	adc	tmpidx1, tmpidx1					/* 1 */

	/* add table offset */
	subi	tmpidx0, lo8(-(table))					/* 1 */
	sbci	tmpidx1, hi8(-(table))					/* 1 */

	/* read 24(32)bit counter into (r27:)r26:r25:r24 (r31:r30 = Z) */
	ldd	tmpcnt0, tmpidxW+0					/* 2 */
	ldd	tmpcnt1, tmpidxW+1					/* 2 */
	ldd	tmpcnt2, tmpidxW+2					/* 2 */
#ifdef HISTOGRAM_DEPTH_32BIT
	ldd	tmpcnt3, tmpidxW+3					/* 2 */
#endif

	/* increase by one */
	subi	tmpcnt0, -1						/* 1 */
	adc	tmpcnt1, __zero_reg__					/* 1 */
	adc	tmpcnt2, __zero_reg__					/* 1 */
#ifdef HISTOGRAM_DEPTH_32BIT
	adc	tmpcnt3, __zero_reg__					/* 1 */
#endif

	/* store back increased 24(32)bit counter into same address */
#ifdef HISTOGRAM_DEPTH_32BIT
	std	tmpidxW+3, tmpcnt3					/* 2 */
#endif
	std	tmpidxW+2, tmpcnt2					/* 2 */
	std	tmpidxW+1, tmpcnt1					/* 2 */
	std	tmpidxW+0, tmpcnt0					/* 2 */

	/* measurement_count++; */
	lds	tmpidx0, measurement_count				/* 2 */
	subi	tmpidx0, -(1)						/* 1 */
	sts	measurement_count, tmpidx0				/* 2 */

	/* restore temp counter */
#ifdef HISTOGRAM_DEPTH_32BIT
	pop	tmpcnt3							/* 2 */
#endif
	pop	tmpcnt2							/* 2 */
	pop	tmpcnt1							/* 2 */
	pop	tmpcnt0							/* 2 */

	/* restore pointer register */
	pop	tmpidx1							/* 2 */
	pop	tmpidx0							/* 2 */

	/* restore SREG */
	out	_SFR_IO_ADDR(SREG), sreg_save				/* 1 */

	/* Reset the interrupt flag: EIFR |= BIT(INTF0) */
	sbi	_SFR_IO_ADDR(EIFR), INTF0				/* 2 */

	reti								/* 5 */

/* TOTAL CPU CYCLES:
 * my_ADC_vect:      72 cycles for 32bit (9 less for 24 bit, i.e. 63 cycles)
 * C lang ADC_vect:  85 cycles for 32bit
 *
 * 1> [[ 1.0E6*C/F || C<-[63,72,85]] || F<-[16.0E6, 20.0E6]].
 * [[3.9375,4.5,5.3125],[3.15,3.6,4.25]]
 *
 * us      asm24  asm32 C32bit
 * 16MHz:  3.9375 4.5   5.3125
 * 20MHz:  3.15   3.6   4.25
 *
 */
