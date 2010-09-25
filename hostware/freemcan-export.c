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
#include <stdint.h>
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

  char reason = 'X';
  switch (value_table_packet->reason) {
  case PACKET_VALUE_TABLE_DONE:
  case PACKET_VALUE_TABLE_RESEND:
  case PACKET_VALUE_TABLE_ABORTED:
  case PACKET_VALUE_TABLE_INTERMEDIATE:
    reason = value_table_packet->reason;
    break;
  }

  char *prefix = "data";
  switch (value_table_packet->type) {
  case VALUE_TABLE_TYPE_HISTOGRAM:   prefix = "hist"; break;
  case VALUE_TABLE_TYPE_TIME_SERIES: prefix = "time"; break;
  }

  char date[128];
  strftime(date, sizeof(date), "%Y-%m-%d.%H:%M:%S", tm_);
  static char fname[256];
  snprintf(fname, sizeof(fname), "%s.%s.%c.%s", prefix, date, reason, extension);
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


typedef struct {
  double counts;
  double counts_error;
  double duration;
  double avg_cpm;   /**< counts per minute */
  double avg_cpm_error; /**< error in counts per minute */
  double statistical_error;
} statistics_t;


void print_stats(FILE *file, const char *prefix, const char *eol,
                 const statistics_t *s)
{
  fprintf(file, "%sduration:           %.1f seconds = %.2f minutes = %.4f hours%s",
          prefix, s->duration, s->duration/60.0, s->duration/3600.0, eol);
  fprintf(file, "%scounts:             %.0f +- %1.2f counts (avg is within 1 sigma)%s",
          prefix, s->counts, s->counts_error, eol);
  fprintf(file, "%scounts per minute:  %1.2f +- %1.2f cpm (avg is within 1 sigma)%s",
          prefix, s->avg_cpm, s->avg_cpm_error, eol);
  fprintf(file, "%sstatistical error:  %1.1f %%%s",
          prefix, s->statistical_error, eol);
}


bool write_next_intermediate_packet = false;


/* documented in freemcan-export.h */
void export_value_table(const packet_value_table_t *value_table_packet)
{
  const size_t element_count = value_table_packet->element_count;
  FILE *datfile = NULL;
  if (write_next_intermediate_packet ||
      (value_table_packet->reason != PACKET_VALUE_TABLE_INTERMEDIATE)) {
    write_next_intermediate_packet = false;
    const char *fname = export_value_table_get_filename(value_table_packet, "dat");
    datfile = fopen(fname, "w");
    assert(datfile);
    fmlog("Writing value table to file %s", fname);
    const char *reason_str = "unknown type";
    switch (value_table_packet->reason) {
    case PACKET_VALUE_TABLE_DONE: reason_str = "measurement completed"; break;
    case PACKET_VALUE_TABLE_RESEND: reason_str = "resent value table after measurement completed"; break;
    case PACKET_VALUE_TABLE_ABORTED: reason_str = "measurement aborted"; break;
    case PACKET_VALUE_TABLE_INTERMEDIATE: reason_str = "intermediate result"; break;
    }
    fprintf(datfile, "# type:\t'%c' (%s)\n", value_table_packet->reason, reason_str);
  }
  switch (value_table_packet->type) {
  case VALUE_TABLE_TYPE_HISTOGRAM: /* histogram data */
    if (datfile) {
      fprintf(datfile, "# receive_time:\t%lu (%s)\n",
              value_table_packet->receive_time, time_rfc_3339(value_table_packet->receive_time));
      fprintf(datfile, "# element_count:\t%d\n", value_table_packet->element_count);
      fprintf(datfile, "# orig_element_size:\t%d (%d bit)\n",
              value_table_packet->orig_element_size, 8*value_table_packet->orig_element_size);
      fprintf(datfile, "# duration:\t%d\n", value_table_packet->duration);
      fprintf(datfile, "# total_duration:\t%d\n", value_table_packet->total_duration);
      fprintf(datfile, "# max_value:\t%d\n", value_table_packet->max_value);

      for (size_t i=0; i<element_count; i++) {
        fprintf(datfile, "%d\t%u\n", i, value_table_packet->elements[i]);
      }
    }
    break;
  case VALUE_TABLE_TYPE_TIME_SERIES: /* series of counter data */
    if (1) {
      const uint32_t elapsed_time = value_table_packet->duration +
        (value_table_packet->total_duration * (value_table_packet->element_count - 1));

      if (datfile) {
        fprintf(datfile, "# number of intervals:    %d\n",     value_table_packet->element_count);
        fprintf(datfile, "# duration per interval:  %d sec\n", value_table_packet->total_duration);

        fprintf(datfile, "# receive_time:           %lu (%s)\n",
                value_table_packet->receive_time, time_rfc_3339(value_table_packet->receive_time));
        fprintf(datfile, "# orig_element_size:      %d byte (%d bit)\n",
                value_table_packet->orig_element_size, 8*value_table_packet->orig_element_size);
        fprintf(datfile, "# duration per value:     %u\n", value_table_packet->total_duration);
        fprintf(datfile, "# duration of last value: %u\n", value_table_packet->duration);
        fprintf(datfile, "# elapsed time:           %u sec\n", elapsed_time);
      }

      uint_least32_t total_count = 0;
      for (size_t i=0; i<element_count; i++) {
        total_count += value_table_packet->elements[i];
      }

      statistics_t s;
      s.counts = total_count;
      s.counts_error = sqrt(s.counts);
      s.duration = (double)(elapsed_time);
      s.avg_cpm = 60.0*s.counts/s.duration;
      s.avg_cpm_error = s.avg_cpm*s.counts_error/s.counts;
      s.statistical_error = 100.0 * s.avg_cpm_error / s.avg_cpm;

      print_stats(stdout,  "     ", "\r\n", &s);
      if (datfile) {
        print_stats(datfile, "# ",    "\n",   &s);
      }
    }
    break;
  }

  if (datfile) {
    fclose(datfile);
  }
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
