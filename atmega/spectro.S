#include <avr/io.h>
#include "registers.h"

#if defined(__AVR_ATmega644__) || defined(__AVR_ATmega644P__)
#else
# error Unsupported MCU!
#endif

//#define HISTOGRAM_24BIT

	/* FIXME: __zero_reg__ definition should have happened already! */
#define __zero_reg__ r1

.global my_ADC_vect
my_ADC_vect:
	in	sreg_save, _SFR_IO_ADDR(SREG)				/* 1 */

	/* Z pointer register */
	push	r30							/* 2 */
	push	r31							/* 2 */

	/* stores table offset */
	push	r20							/* 2 */
	push	r21							/* 2 */

	/* histogram counter value */
	push	r24							/* 2 */
	push	r25							/* 2 */
	push	r26							/* 2 */
#ifndef HISTOGRAM_24BIT
	push	r27							/* 2 */
#endif

	/* Read 8bit value from ADC, extend to 16bit value */
	lds	r30, ADCH	// Read highest 8 bits of ADC value a	/* 2 */
	clr	r31		// Throw away the lowest 2 bits		/* 1 */

	/* multiply a by 4 to get table offset */
	add	r30, r30	// (2*a)				/* 1 */
	adc	r31, r31						/* 1 */
	add	r30, r30	// (4*a)				/* 1 */
	adc	r31, r31						/* 1 */

	/* load table offset */
	ldi	r20, lo8(table)						/* 1 */
	ldi	r21, hi8(table)						/* 1 */

	/* add table offset */
	add	r30, r24						/* 1 */
	adc	r31, r25						/* 1 */

	/* read 24(32)bit counter into (r27:)r26:r25:r24 (r31:r30 = Z) */
	ld	r24, Z+							/* 2 */
	ld	r25, Z+							/* 2 */
#ifdef HISTOGRAM_24BIT
	ld	r26, Z							/* 2 */
#else
	ld	r26, Z+							/* 2 */
	ld	r27, Z							/* 2 */
#endif

	/* increase by one */
	adiw	r24, 1							/* 2 */
	adc	r26, __zero_reg__					/* 1 */
#ifndef HISTOGRAM_24BIT
	adc	r27, __zero_reg__					/* 1 */
#endif

	/* store back increased 24(32)bit counter into same address */
#ifdef HISTOGRAM_24BIT
	st	Z, r26							/* 2 */
#else
	st	Z, r27							/* 2 */
	st	-Z, r26							/* 2 */
#endif
	st	-Z, r25							/* 2 */
	st	-Z, r24							/* 2 */

#ifndef HISTOGRAM_24BIT
	pop	r27							/* 2 */
#endif
	pop	r26							/* 2 */
	pop	r25							/* 2 */
	pop	r24							/* 2 */

	pop	r21							/* 2 */
	pop	r20							/* 2 */

	pop	r31							/* 2 */
	pop	r30							/* 2 */
	out	_SFR_IO_ADDR(SREG), sreg_save				/* 1 */
	reti								/* 5 */

/* TOTAL CPU CYCLES:
 * my_ADC_vect:      70 cycles for 32bit (9 less for 24 bit)
 * C lang ADC_VECT:  73 cycles
 */


 */
