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
	in	sreg_save, _SFR_IO_ADDR(SREG)

	/* Z pointer register */
	push	r30
	push	r31

	/* stores table offset */
	push	r20
	push	r21

	/* histogram counter value */
	push	r24
	push	r25
	push	r26
#ifndef HISTOGRAM_24BIT
	push	r27
#endif

	/* Read 8bit value from ADC, extend to 16bit value */
	lds	r30, ADCH	// Read highest 8 bits of ADC value a
	clr	r31		// Throw away the lowest 2 bits

	/* multiply a by 4 to get table offset */
	add	r30, r30	// (2*a)
	adc	r31, r31
	add	r30, r30	// (4*a)
	adc	r31, r31

	/* load table offset */
	ldi	r20, lo8(table)
	ldi	r21, hi8(table)

	/* add table offset */
	add	r30, r24
	adc	r31, r25

	/* read 24(32)bit counter into (r27:)r26:r25:r24 (r31:r30 = Z) */
	ld	r24, Z+
	ld	r25, Z+
#ifdef HISTOGRAM_24BIT
	ld	r26, Z
#else
	ld	r26, Z+
	ld	r27, Z
#endif

	/* increase by one */
	adiw	r24, 1
	adc	r26, __zero_reg__
#ifndef HISTOGRAM_24BIT
	adc	r27, __zero_reg__
#endif

	/* store back increased 24(32)bit counter into same address */
#ifdef HISTOGRAM_24BIT
	st	Z, r26
#else
	st	Z, r27
	st	-Z, r26
#endif
	st	-Z, r25
	st	-Z, r24

#ifndef HISTOGRAM_24BIT
	pop	r27
#endif
	pop	r26
	pop	r25
	pop	r24

	pop	r21
	pop	r20

	pop	r31
	pop	r30
	out	_SFR_IO_ADDR(SREG), sreg_save
	reti

	/* Old first shot at interrupt handling routine */

/* Register map:
 * r0
 * r1
 * r2
 * r3
 * r4
 * r5
 * r6
 * r7
 * r8
 * r9
 * r10
 * r11
 * r12
 * r13
 * r14
 * r15 - used by ext_inthandler to increment counter in
 * r16 - constant 0
 * r17 - used by ext_inthandler to save SREG
 * r18
 * r19
 * r20
 * r21
 * r22
 * r23
 * r24 - used by ext_inthandler to increment counter in
 * r25 - used by ext_inthandler to increment counter in
 * r26 - used by ext_inthandler for counter address
 * r27 - used by ext_inthandler for counter address
 * r28
 * r29
 * r30
 * r31
 */

ext_inthandler:
	/* interrupt housekeeping */
	lds	r17, SREG /* INCORRECT */

	/* read 8bit value from external ADC, extend to 16bit value */
	lds	r26, ADCH
	clr	r27

	/* multiply index with 4 to get counter addr */
	lsl	r26
	rol	r27
	lsl	r26
	rol	r27

	/* read 24bit counter into r15:r25:r24 (r27:r26 = X) */
	ld	r24, X+
	ld	r25, X+
	ld	r15, X

	/* increase by one */
	adiw	r24, 0x0001
	adc	r10, r16

	/* store 24bit counter into same address */
	st	X, r15
	st	-X, r25
	st	-X, r24

	/* housekeeping */
	sts	SREG, r17
	reti
