/** \file code-comparison/histogram.c
 * \brief Generic histogram code for comparing the generated machine code
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
