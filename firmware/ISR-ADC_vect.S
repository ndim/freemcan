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
#include "global.h"

#if defined(__AVR_ATmega644__) || defined(__AVR_ATmega644P__)
#else
# error Unsupported MCU!
#endif

/** \bug Cause compile time error when ELEMENT_SIZE_IN_BYTES != 3. */

/* define register names in a single place */
#define tmpcnt0 r24
#define tmpcnt1 r25
#define tmpcnt2 r26
#define tmpcnt3 r27

#define tmpidx0 r30
#define tmpidx1 r31

#define tmpidxW Z

/* avr-gcc docs say that __zero_reg__ is always r1 */
#define __zero_reg__ r1

.extern	table

/* Rename to ADC_vect if you want to actually link this
 * version into the firmware.
 *
 * Also, DO re-introdurce the sreg_save definition and register reservation
 * to global.h before linking this ISR into the firmware image.
 */
.global moo_ADC_vect
moo_ADC_vect:	/* interrupt entry costs another 5 cycles */		/* 5 */

	/* save SREG */
	in	sreg_save, _SFR_IO_ADDR(SREG)				/* 1 */

	/* save pointer register */
	push	tmpidx0							/* 2 */
	push	tmpidx1							/* 2 */

	/* save temp counter */
	push	tmpcnt0							/* 2 */
	push	tmpcnt1							/* 2 */
	push	tmpcnt2							/* 2 */

	/* pull pin at bit 6 to discharge peak hold capacitor */
	sbi	_SFR_IO_ADDR(PORTD), 6					/* 2 */

	/* Read analog value from ADC */
	lds	tmpcnt1, ADCH	// Read high 8 bits of ADC value a	/* 2 */
	lds	tmpcnt0, ADCL	// Read low 8 bits of ADC value a	/* 2 */

	/* multiply a by 3 to get offset within table */
	movw	tmpidx0, tmpcnt0					/* 1 */
	add	tmpidx0, tmpidx0					/* 1 */
	adc	tmpidx1, tmpidx1					/* 1 */
	add	tmpidx0, tmpcnt0					/* 1 */
	adc	tmpidx1, tmpcnt1					/* 1 */

	/* add table offset */
	subi	tmpidx0, lo8(-(table))					/* 1 */
	sbci	tmpidx1, hi8(-(table))					/* 1 */

	/* read 24(32)bit counter into (r27:)r26:r25:r24 (r31:r30 = Z) */
	ldd	tmpcnt0, tmpidxW+0					/* 2 */
	ldd	tmpcnt1, tmpidxW+1					/* 2 */
	ldd	tmpcnt2, tmpidxW+2					/* 2 */

	/* increase by one */
	adiw	tmpcnt0, 1						/* 2 */
	adc	tmpcnt2, __zero_reg__					/* 1 */

	/* store back increased 24(32)bit counter into same address */
	std	tmpidxW+2, tmpcnt2					/* 2 */
	std	tmpidxW+1, tmpcnt1					/* 2 */
	std	tmpidxW+0, tmpcnt0					/* 2 */

	/* set pin to GND and release peak hold capacitor */
	cbi	_SFR_IO_ADDR(PORTD), 6					/* 2 */

	/* restore temp counter */
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


/* Total cycles:
 *   Assembly: 64
 *   C:        70
 */
