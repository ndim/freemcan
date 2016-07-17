/** \file firmware/bin_invented-histogram.h
 * \brief Global adjustments for freemcan firmware
 *
 * \author Copyright (C) 2010 samplemaker
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
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
 * \todo Add bin_invented-histogram.S to doxygen somehow (doxygen cannot read assembly language by default).
 *
 * \addtogroup perso_sim_mac
 * @{
 */

#ifndef BIN_INVENTED_HISTOGRAM_H
#define BIN_INVENTED_HISTOGRAM_H

#include <stdlib.h>


/** Simulated symbol for the size of the #data_table object.
 *
 * This simulated symbol contains the size of #data_table object as
 * the value of its absolute address. The linker can fill in this
 * value in code, so you can use this as an initialization value in C
 * code - given the proper type casting (see #DATA_TABLE_SIZE).
 */
extern char data_table_size_as_addr[];


/** The correct type casting for #data_table_size_as_addr.
 */
#define DATA_TABLE_SIZE ((size_t)((void *)data_table_size_as_addr))


#endif /* BIN_INVENTED_HISTOGRAM_H */


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
