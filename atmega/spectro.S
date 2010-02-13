#include <avr/io.h>
#include "registers.h"

#if defined(__AVR_ATmega644__) || defined(__AVR_ATmega644P__)
#else
# error Unsupported MCU!
#endif

.global my_ADC_vect
my_ADC_vect:
	in	sreg_save, _SFR_IO_ADDR(SREG)
	push	r26
	push	r27

	in	r0, 0 // ADCH	// Read highest 8 bits of ADC value (a).
	clr	r27		// Throw away the lowest 2 bits.

	lsl	r26		// (2*a)
	rol	r27

	lsl	r26		// (4*a)
	rol	r27

	

	pop	r27
	pop	r26
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
	lds	r17, SREG

	/* read 8bit value from external ADC, extend to 16bit value */
	lds	r26, ADC_BITS_PORT
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
