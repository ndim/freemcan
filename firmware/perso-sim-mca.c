/** \file firmware/perso-sim-mca.c
 * \brief Personality: MCA simulation with invented histogram
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
 * \defgroup perso_sim_mac Personality: MCA simulation with invented histogram
 * \ingroup firmware_personality_groups
 *
 * @{
 */


#define BYTES_PER_VALUE 3
#define BITS_PER_VALUE (BYTES_PER_VALUE * 8)

#include "global.h"
#include "main.h"
#include "data-table.h"

#include "res_invented-histogram.h"


/** See * \see data_table */
data_table_info_t data_table_info = {
  /** Actual size of #data_table in bytes */
  sizeof(data_table),
  /** Type of value table we send */
  VALUE_TABLE_TYPE_HISTOGRAM,
  /** Table element size */
  BITS_PER_VALUE,
};

/** See * \see data_table */
PERSONALITY("sim-mca",
            2,0,
            1,
            sizeof(data_table),
            BITS_PER_VALUE);


/** \todo Should give out a reasonable value */
uint16_t get_duration(void)
{
  return 0;
}

void personality_start_measurement_sram(void)
{
}

void on_measurement_finished(void)
{
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
