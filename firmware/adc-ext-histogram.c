/** \file firmware/adc-ext-histogram.c
 * \brief Histogram table for use with external ADC
 *
 * \author Copyright (C) 2010 samplemaker
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
 *
 * \defgroup adc_ext_histogram Histogram table for use with external ADC
 * \ingroup firmware
 *
 * Histogram table for use with external ADC.
 *
 * @{
 */


#include "adc-ext-histogram.h"


/** Actual size of #table in bytes
 *
 * Unfortunately, we need to define this variable and use it.  It
 * would be so nice if we could just use the ELF symbol size of
 * table[] to determine the size, but, alas, we do not know how to do
 * that.
 *
 * \see data_table
 */
const size_t sizeof_table = sizeof(table);


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
