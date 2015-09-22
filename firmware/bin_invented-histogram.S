/* NOTE: This file is not actually parsed by doxygen.
 *       See bin_invented-histogram.h for the doxygen documentation for this file.
 */

/** \file firmware/bin_invented-histogram.S
 * \brief Assembly language inclusion of invented histogram data as RO data
 *
 * \author Copyright (C) 2015 Hans Ulrich Niedermann <hun@n-dimensional.de>
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
 *
 *
 * Using this assembly file for linking binary data into the
 * executable has a few advantages over other methods:
 *
 *   * put the binary data into the proper linker section
 *   * have proper symbol size for the binary data
 *   * have the linker replace the data table size in code
 *     with the actual value
 *
 */

	.section .rodata


	.global	data_table
	.type	data_table, @object
	/* AVR is 8bit, so we do not care about alignment for data_table.
	 * Otherwise, a platform specific .balign value would be called for.
	 */
data_table:
	.incbin	".objs/invented-histogram.bin"
	.size	data_table, . - data_table


	.global data_table_size
	.type	data_table_size, @object
data_table_size = (. - data_table)
	.size	data_table_size, 0


/*
 * Local Variables:
 * mode: asm
 * End:
 */
