#include "elt.h"

#define MAX_IDX 0x100

Element32 e32[MAX_IDX];
Element24 e24[MAX_IDX];
Element16 e16[MAX_IDX];
Element8  e8[MAX_IDX];
int       i0[MAX_IDX];

void test(void)
{
	++e8[0];
	++e16[0];
	++e24[0];
	++e32[0];
}
