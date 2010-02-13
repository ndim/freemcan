#ifndef REGISTERS_H
#define REGISTERS_H

/* Safe registers to reserve for special purposes are r2..r7, apparently:
 * http://www.nongnu.org/avr-libc/user-manual/FAQ.html#faq_regbind
 */

#ifdef __ASSEMBLER__

/* Define the same special use registers as the C compiler uses below */
# define sreg_save r7

#else

/* Reserve registers for special use. The C compiler will not touch them then! */
register unsigned char sreg_save asm("r7");

#endif /* !__ASSEMBLER__ */

#endif /* !REGISTERS_H */
