/** \file hostware/freemcan-export.c
 * \brief Value table export functions
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
 *
 * \defgroup freemcan_export Export Value Table Files
 * \ingroup hostware_generic
 * @{
 */

#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "freemcan-export.h"
#include "freemcan-log.h"


/* documented in freemcan-export.h */
char *export_value_table_get_filename(const packet_value_table_t *value_table_packet,
                                      const char *extension)
{
  const struct tm *tm_ = localtime(&value_table_packet->receive_time);
  assert(tm_);
  char type = 'X';
  switch (value_table_packet->type) {
  case PACKET_VALUE_TABLE_DONE:
  case PACKET_VALUE_TABLE_RESEND:
  case PACKET_VALUE_TABLE_ABORTED:
  case PACKET_VALUE_TABLE_INTERMEDIATE:
    type = value_table_packet->type;
    break;
  }
  char date[128];
  strftime(date, sizeof(date), "%Y-%m-%d.%H:%M:%S", tm_);
  static char fname[256];
  snprintf(fname, sizeof(fname), "hist.%s.%c.%s", date, type, extension);
  return fname;
}


const char *time_rfc_3339(const time_t time)
{
  const struct tm *tm_ = localtime(&time);
  assert(tm_);
  static char buf[64];
  strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S%z", tm_);
  return buf;
}


/* documented in freemcan-export.h */
void export_value_table(const packet_value_table_t *value_table_packet)
{
  const size_t element_count = value_table_packet->element_count;
  const char *fname = export_value_table_get_filename(value_table_packet, "dat");
  FILE *histfile = fopen(fname, "w");
  assert(histfile);
  fmlog("Writing histogram to file %s", fname);
  const char *type_str = "unknown type";
  switch (value_table_packet->type) {
  case PACKET_VALUE_TABLE_DONE: type_str = "measurement completed"; break;
  case PACKET_VALUE_TABLE_RESEND: type_str = "resent histogram after measurement completed"; break;
  case PACKET_VALUE_TABLE_ABORTED: type_str = "measurement aborted"; break;
  case PACKET_VALUE_TABLE_INTERMEDIATE: type_str = "intermediate result"; break;
  }
  fprintf(histfile, "# type:\t'%c' (%s)\n", value_table_packet->type, type_str);
  /** \todo We need a better way to distinguish between histogram data
   *        and counter/time series data.
   */
  if (element_count >= 32) { /* histogram data */
    fprintf(histfile, "# receive_time:\t%lu (%s)\n",
            value_table_packet->receive_time, time_rfc_3339(value_table_packet->receive_time));
    fprintf(histfile, "# element_count:\t%d\n", value_table_packet->element_count);
    fprintf(histfile, "# orig_element_size:\t%d (%d bit)\n",
            value_table_packet->orig_element_size, 8*value_table_packet->orig_element_size);
    fprintf(histfile, "# duration:\t%d\n", value_table_packet->duration);
    fprintf(histfile, "# total_duration:\t%d\n", value_table_packet->total_duration);
    fprintf(histfile, "# max_value:\t%d\n", value_table_packet->max_value);

    for (size_t i=0; i<element_count; i++) {
      fprintf(histfile, "%d\t%u\n", i, value_table_packet->elements[i]);
    }
  } else { /* counter data */
    fprintf(histfile, "# number of intervalls:\t%d\n", value_table_packet->element_count);
    fprintf(histfile, "#duration per interval: %d sec\n", value_table_packet->total_duration);

    unsigned long total_count = 0;
    for (size_t i=0; i<element_count; i++) {
      total_count += value_table_packet->elements[i];
    }

    fprintf(histfile, "#elapsed time:      %d sec\n",
            value_table_packet->element_count*value_table_packet->total_duration);

    fprintf(histfile, "#total counts:      %lu", total_count);

    float total_counts_error = sqrt((float)(total_count));
    fprintf(histfile, "    +/- %1.2f CNTs (average is within 1 sigma)\n", total_counts_error);

    float average_count_rate = 60.0*(float)(total_count)/(float)(element_count*value_table_packet->total_duration);
    fprintf(histfile, "#counts per minute: %1.2f", average_count_rate);

    float average_counts_error = average_count_rate*total_counts_error/total_count;
    fprintf(histfile, "  +/- %1.2f CPMs (average is within 1 sigma)\n", average_counts_error);

    fprintf(histfile, "#statistical error: %1.1f %%\n",
            100.0*average_counts_error/average_count_rate);
  }
  fclose(histfile);
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
