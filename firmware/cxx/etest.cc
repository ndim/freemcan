#include "elt.h"

Element32 e32[16];
Element24 e24[16];
Element16 e16[16];
Element8  e8[16];
int       i0[16];

void test(void)
{
	++e8[0];
	++e16[0];
	++e24[0];
	++e32[0];
}
