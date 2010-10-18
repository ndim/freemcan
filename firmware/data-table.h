/** \file firmware/data-table.h
 * \brief Data table interface between MCA/time series and send_histogram()
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
 * \defgroup data_table Data table interface between MCA/time series and send_table()
 * \ingroup firmware
 *
 * @{
 */

#ifndef DATA_TABLE_H
#define DATA_TABLE_H


#include <stdlib.h>


/** The data table as an opaque array of bytes
 *
 * The actual structure of the bytes in the table is undefined for the
 * purpose of this interface.
 */
extern char data_table[];


/** The size of the data #table in bytes */
extern size_t sizeof_data_table;


/** @} */

#endif /* DATA_TABLE_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
