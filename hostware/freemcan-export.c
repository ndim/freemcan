/** \file hostware/freemcan-export.c
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
 *
 * \defgroup freemcan_export Export Histogram Files
 * \ingroup hostware_generic
 * @{
 */

#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

#include <time.h>

#include "freemcan-export.h"
#include "freemcan-log.h"


/* documented in freemcan-export.h */
char *export_histogram_get_filename(const packet_histogram_t *histogram_packet,
                                    const char *extension)
{
  const struct tm *tm_ = localtime(&histogram_packet->receive_time);
  assert(tm_);
  char type = 'X';
  switch (histogram_packet->type) {
  case PACKET_HISTOGRAM_DONE:
  case PACKET_HISTOGRAM_RESEND:
  case PACKET_HISTOGRAM_ABORTED:
  case PACKET_HISTOGRAM_INTERMEDIATE:
    type = histogram_packet->type;
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
void export_histogram(const packet_histogram_t *histogram_packet)
{
  const size_t element_count = histogram_packet->element_count;
  const char *fname = export_histogram_get_filename(histogram_packet, "dat");
  FILE *histfile = fopen(fname, "w");
  assert(histfile);
  fmlog("Writing histogram to file %s", fname);
  const char *type_str = "unknown type";
  switch (histogram_packet->type) {
  case PACKET_HISTOGRAM_DONE: type_str = "measurement completed"; break;
  case PACKET_HISTOGRAM_RESEND: type_str = "resent histogram after measurement completed"; break;
  case PACKET_HISTOGRAM_ABORTED: type_str = "measurement aborted"; break;
  case PACKET_HISTOGRAM_INTERMEDIATE: type_str = "intermediate result"; break;
  }
  fprintf(histfile, "# type:\t'%c' (%s)\n", histogram_packet->type, type_str);
  fprintf(histfile, "# receive_time:\t%lu (%s)\n",
          histogram_packet->receive_time, time_rfc_3339(histogram_packet->receive_time));
  fprintf(histfile, "# element_count:\t%d\n", histogram_packet->element_count);
  fprintf(histfile, "# orig_element_size:\t%d (%d bit)\n",
          histogram_packet->orig_element_size, 8*histogram_packet->orig_element_size);
  fprintf(histfile, "# duration:\t%d\n", histogram_packet->duration);
  fprintf(histfile, "# total_duration:\t%d\n", histogram_packet->total_duration);
  fprintf(histfile, "# max_value:\t%d\n", histogram_packet->max_value);
  for (size_t i=0; i<element_count; i++) {
    fprintf(histfile, "%d\t%u\n", i, histogram_packet->elements[i]);
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
