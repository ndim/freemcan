#include <stdint.h>

#include "histogram.h"

#define MAX_ENTRIES 1024

volatile uint32_t histogram[MAX_ENTRIES];
volatile uint8_t  total_counter8;
volatile uint16_t total_counter16;
volatile uint32_t total_counter32;
volatile uint64_t total_counter64;

void histogram_init(void)
{
	for (unsigned int i=0; i<MAX_ENTRIES; i++) {
		histogram[i] = 0;
	}
	total_counter8  = 0;
	total_counter16 = 0;
	total_counter32 = 0;
	total_counter64 = 0;
}

void histogram_update(const uint16_t value)
{
	histogram[value]++;
	total_counter8++;
	total_counter16++;
	total_counter32++;
	total_counter64++;
}
