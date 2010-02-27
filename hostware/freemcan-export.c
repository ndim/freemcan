/** \file freemcan-export.c
 * \brief histogram export functions
 *
 * \author Copyright (C) 2010 samplemaker
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

#include <fcntl.h>
#include <stdio.h>

#include "freemcan-export.h"

/** Dump the last histogram into freemcan-export.dat
 *
 * Use this with the helper utility "plot-freemcan.plt":
 * type
 * gnuplot load 'plot_freemcan.plt'
 * on the shell
 */

void export_histogram(const packet_histogram_t *histogram_packet){

  const size_t element_count = histogram_packet->element_count;
  const size_t element_size = histogram_packet->element_size;

  FILE *fdexport = fopen("freemcan-export.dat", "w");

  /* fprintf(fdexport, "ChannelNo\tCounts\n"); */

  if (element_size == 4){
    uint32_t count;
    for (uint16_t i = 0; i < element_count; i++){
        count = histogram_packet->elements.e32[i];
        fprintf(fdexport, "%d\t%d\n", i, count);
    };
  }
  else
  if (element_size == 2){
    uint16_t count;
    for (uint16_t i = 0; i < element_count; i++){
        count = histogram_packet->elements.e16[i];
        fprintf(fdexport, "%d\t%d\n", i, count);
    };
  }
  else
  {
    uint8_t count;
    for (uint16_t i = 0; i < element_count; i++){
        count = histogram_packet->elements.e8[i];
        fprintf(fdexport, "%d\t%d\n", i, count);
    };
  }

  fclose (fdexport);
}

