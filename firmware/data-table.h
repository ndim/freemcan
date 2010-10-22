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

#include "packet-defs.h"


/** The data table as an opaque array of bytes
 *
 * The actual structure of the bytes in the table is undefined for the
 * purpose of this interface.
 */
extern char data_table[];


typedef struct {
  /** The size of the #data_table in bytes */
  size_t size;

  /** The type of value table */
  packet_value_table_type_t type;

  /** The size of a single table element */
  uint8_t element_size;

  /** Total number of bytes in table */
  size_t total_size;
} data_table_info_t;


extern data_table_info_t data_table_info;


/** set by main(), read by send_table(), unused by anybody else */
extern uint32_t token;


/** @} */

#endif /* DATA_TABLE_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
