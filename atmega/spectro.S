#include <avr/io.h>
#include "registers.h"

#if defined(__AVR_ATmega644__) || defined(__AVR_ATmega644P__)
#else
# error Unsupported MCU!
#endif

#define HISTOGRAM_DEPTH_32BIT

	/* FIXME: __zero_reg__ definition should have happened already! */
#define __zero_reg__ r1

.global my_ADC_vect
my_ADC_vect:	/* interrupt entry costs another 5 cycles */		/* 5 */
	in	sreg_save, _SFR_IO_ADDR(SREG)				/* 1 */

	/* Z pointer register */
	push	r30							/* 2 */
	push	r31							/* 2 */

	/* histogram counter value */
	push	r24							/* 2 */
	push	r25							/* 2 */
	push	r26							/* 2 */
#ifdef HISTOGRAM_DEPTH_32BIT
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

	/* add table offset */
	subi	r30, lo8(-(table))					/* 1 */
	sbci	r31, hi8(-(table))					/* 1 */

	/* read 24(32)bit counter into (r27:)r26:r25:r24 (r31:r30 = Z) */
	ldd	r24, Z+0						/* 2 */
	ldd	r25, Z+1						/* 2 */
	ldd	r26, Z+2						/* 2 */
#ifdef HISTOGRAM_DEPTH_32BIT
	ldd	r27, Z+3						/* 2 */
#endif

	/* increase by one */
	adiw	r24, 1							/* 2 */
	adc	r26, __zero_reg__					/* 1 */
#ifdef HISTOGRAM_DEPTH_32BIT
	adc	r27, __zero_reg__					/* 1 */
#endif

	/* store back increased 24(32)bit counter into same address */
#ifdef HISTOGRAM_DEPTH_32BIT
	std	Z+3, r26						/* 2 */
#endif
	std	Z+2, r26						/* 2 */
	std	Z+1, r25						/* 2 */
	std	Z+0, r24						/* 2 */

	/* measurement_count++; */
	lds	r24, measurement_count					/* 2 */
	subi	r24, -(1)						/* 1 */
	sts	measurement_count, r24					/* 2 */

#ifdef HISTOGRAM_DEPTH_32BIT
	pop	r27							/* 2 */
#endif
	pop	r26							/* 2 */
	pop	r25							/* 2 */
	pop	r24							/* 2 */

	pop	r31							/* 2 */
	pop	r30							/* 2 */
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
