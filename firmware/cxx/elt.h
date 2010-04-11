#ifndef ELT_H
#define ELT_H

#include <stdint.h>

#ifdef __cplusplus
#endif

class Element8 {
private:
  uint8_t data;
public:
  Element8& operator++() {
    ++data;
  };
};

class Element16 {
private:
  uint16_t data;
public:
  Element16& operator++() {
    ++data;
  };
};

class Element24 {
private:
  uint8_t data[3];
public:
  Element24& operator++() {
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
	  "z" (data)
	: "r24", "r25"
	);
  };
};

class Element32 {
private:
  uint32_t data;
public:
  Element32& operator++() {
    ++data;
  };
};

#ifdef __cplusplus
#endif

#endif
