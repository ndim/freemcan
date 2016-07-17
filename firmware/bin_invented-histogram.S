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
	/* AVR is 8bit, so we do not care about alignment for
	 * data_table on AVR.
	 * Otherwise, a platform specific .balign value would be called for.
	 * Getting the platform specific aligment value programmatically is
	 * a bit difficult, though. So we would just err on the side of
	 * alignment and align to multiples of 8 if we were to support
	 * non-AVR platforms here.
	 *
	 * .balign	8
	 */
data_table:
	.incbin	BIN_FILENAME
	.size	data_table, . - data_table

	/* Do not place any objects between data_table and
	 * data_table_size_as_addr here. We must not change . here. */

	/* Pseudo object whose address can be used as size constant */
	.global data_table_size_as_addr
	.type	data_table_size_as_addr, @object
data_table_size_as_addr = (. - data_table)
	.size	data_table_size_as_addr, 0


/*
 * Local Variables:
 * mode: asm
 * End:
 */
