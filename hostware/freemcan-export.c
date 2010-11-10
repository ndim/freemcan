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
  case VALUE_TABLE_TYPE_SAMPLES:     prefix = "samp"; break;
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


static
void export_common_vtable(FILE *datfile,
                          const packet_value_table_t *value_table_packet)
{
  if (datfile) {
    const char *type_str = "unknown data type";
    switch (value_table_packet->type) {
    case VALUE_TABLE_TYPE_HISTOGRAM:
      type_str = "histogram"; break;
    case VALUE_TABLE_TYPE_TIME_SERIES:
      type_str = "time series"; break;
    case VALUE_TABLE_TYPE_SAMPLES:
      type_str = "samples"; break;
    }
    fprintf(datfile, "# value table type:         '%c' (%s)\n",
            value_table_packet->type, type_str);

    const char *reason_str = "unknown type";
    switch (value_table_packet->reason) {
    case PACKET_VALUE_TABLE_DONE:
      reason_str = "measurement completed"; break;
    case PACKET_VALUE_TABLE_RESEND:
      reason_str = "resent value table after measurement completed"; break;
    case PACKET_VALUE_TABLE_ABORTED:
      reason_str = "measurement aborted"; break;
    case PACKET_VALUE_TABLE_INTERMEDIATE:
      reason_str = "intermediate result"; break;
    }
    fprintf(datfile, "# reason:                   '%c' (%s)\n",
            value_table_packet->reason, reason_str);

    const time_t start_time = (value_table_packet->token)?
      *((const time_t *)value_table_packet->token) : 0 ;
    fprintf(datfile, "# start_time:               %lu (%s)\n",
            start_time, time_rfc_3339(start_time));

    const time_t receive_time = value_table_packet->receive_time;
    fprintf(datfile, "# receive_time:             %lu (%s)\n",
            receive_time, time_rfc_3339(receive_time));

    const size_t orig_element_size = value_table_packet->orig_element_size;
    fprintf(datfile, "# orig_element_size:        %d byte (%d bit)\n",
            orig_element_size, 8*orig_element_size);
  }
}


static
void export_histogram_vtable(FILE *datfile, const packet_value_table_t *value_table_packet)
{
  if (datfile) {
    const size_t element_count = value_table_packet->element_count;
    uint32_t max_value = 0;
    for (size_t i=0; i<element_count; i++) {
      const uint32_t v = value_table_packet->elements[i];
      if ((i+1<element_count) && (v > max_value)) {
        max_value = v;
      }
      }
    fprintf(datfile, "# element_count:            %d\n",
            element_count);
    fprintf(datfile, "# time elapsed since start: %d\n",
            value_table_packet->duration);
    fprintf(datfile, "# total_duration:           %d\n",
            value_table_packet->total_duration);
    fprintf(datfile, "channel\tcount\n");
    for (size_t i=0; i<element_count; i++) {
      fprintf(datfile, "%d\t%u\n", i, value_table_packet->elements[i]);
    }
  }
}


static
void time_series_stats(FILE *datfile, const char *prefix, const char *eol,
                       const statistics_t *s)
{
  fprintf(datfile, "%sTotal statistics (so far)%s", prefix, eol);
  fprintf(datfile, "%s  total duration:         %.1f seconds = %.2f minutes = %.4f hours%s",
          prefix, s->duration, s->duration/60.0, s->duration/3600.0, eol);
  fprintf(datfile, "%s  total counts:           %.0f +- %1.2f counts (avg is within 1 sigma)%s",
          prefix, s->counts, s->counts_error, eol);
  fprintf(datfile, "%s  counts per minute:      %1.2f +- %1.2f cpm (avg is within 1 sigma)%s",
          prefix, s->avg_cpm, s->avg_cpm_error, eol);
  fprintf(datfile, "%s  statistical error:      %1.1f %%%s",
          prefix, s->statistical_error, eol);
}


static
void export_time_series_vtable(FILE *datfile,
                               const personality_info_t *personality_info,
                               const packet_value_table_t *value_table_packet)
{
  const size_t element_count = value_table_packet->element_count;
  const uint32_t elapsed_time = value_table_packet->duration +
    (value_table_packet->total_duration * (value_table_packet->element_count - 1));

  uint_least32_t total_count = 0;
  uint32_t max_value = 0;
  uint32_t min_value = UINT32_MAX;
  for (size_t i=0; i<element_count; i++) {
    const uint32_t v = value_table_packet->elements[i];
    if (v > max_value) {
      max_value = v;
    }
    switch (value_table_packet->reason) {
    case PACKET_VALUE_TABLE_DONE:
    case PACKET_VALUE_TABLE_RESEND:
      min_value = v;
      break;
    case PACKET_VALUE_TABLE_ABORTED:
    case PACKET_VALUE_TABLE_INTERMEDIATE:
      /* possibly ignore the possibly incomplete value */
      if ((i+1<element_count) || (value_table_packet->total_duration == value_table_packet->duration)) {
        if (v < min_value) {
          min_value = v;
        }
      }
      break;
    }
    total_count += v;
  }

  if (datfile) {
    fprintf(datfile, "# time elapsed since start: %u sec\n", elapsed_time);
    fprintf(datfile, "# minimum value:            %u\n", min_value);
    fprintf(datfile, "# maximum value:            %u\n", max_value);

    fprintf(datfile, "# measurements done:        %u\n", element_count);
    const size_t total_element_count =
      (personality_info->sizeof_table / personality_info->sizeof_value);
    const size_t elements_to_go = total_element_count - element_count;
    fprintf(datfile, "# measurements to do:       %u\n", elements_to_go);
    fprintf(datfile, "# space for measurements:   %u\n", total_element_count);

    fprintf(datfile, "# time per measurement:     %u sec\n",
            value_table_packet->total_duration);
    fprintf(datfile, "# time for last meas'mt:    %u\n",
            value_table_packet->duration);
    const double time_to_go = elements_to_go * value_table_packet->total_duration;
    fprintf(datfile, "# time to go:               %.1f seconds = %.2f minutes = %.4f hours\n",
            time_to_go, time_to_go/60.0f, time_to_go/3600.0f);
  }

  statistics_t s;
  s.counts = total_count;
  s.counts_error = sqrt(s.counts);
  s.duration = (double)(elapsed_time);
  s.avg_cpm = 60.0*s.counts/s.duration;
  s.avg_cpm_error = s.avg_cpm*s.counts_error/s.counts;
  s.statistical_error = 100.0 * s.avg_cpm_error / s.avg_cpm;

  time_series_stats(stdout,  "<    ", "\r\n", &s);
  if (datfile) {
    time_series_stats(datfile, "# ",    "\n",   &s);

    const time_t tdur  = value_table_packet->total_duration;
    fprintf(datfile, "%s\t%s\t%s\t%s\n", "idx", "counts", "time_t", "strftime");
    const time_t start_time = (value_table_packet->token)?
      *((const time_t *)value_table_packet->token) : 0 ;
    for (size_t i=0; i<element_count; i++) {
      const time_t ts = start_time + i * tdur;
      const char *st = time_rfc_3339(ts);
      fprintf(datfile, "%u\t%u\t%ld\t%s\n", i, value_table_packet->elements[i], ts, st);
    }
  }
}


static
void export_samples_vtable(FILE *datfile,
                           const packet_value_table_t *value_table_packet)
{
  uint32_t max_value = 0;
  uint32_t min_value = UINT32_MAX;
  const size_t element_count = value_table_packet->element_count;
  for (size_t i=0; i<element_count; i++) {
    const uint32_t v = value_table_packet->elements[i];
    if (v > max_value) {
      max_value = v;
    }
    if (v < min_value) {
      min_value = v;
    }
  }
  if (datfile) {
    fprintf(datfile, "# minimum value:            %u\n", min_value);
    fprintf(datfile, "# maximum value:            %u\n", max_value);
    for (size_t i=0; i<element_count; i++) {
      /** \todo Write timestamps */
      fprintf(datfile, "%u\t%u\n", i, value_table_packet->elements[i]);
    }
  }
}


bool write_next_intermediate_packet = false;


/* documented in freemcan-export.h */
void export_value_table(const personality_info_t *personality_info,
                        const packet_value_table_t *value_table_packet)
{
  FILE *datfile = NULL;
  if (write_next_intermediate_packet ||
      (value_table_packet->reason != PACKET_VALUE_TABLE_INTERMEDIATE)) {
    write_next_intermediate_packet = false;
    const char *fname = export_value_table_get_filename(value_table_packet, "dat");
    datfile = fopen(fname, "w");
    assert(datfile);
    fmlog("Writing value table to file %s", fname);
  }

  export_common_vtable(datfile, value_table_packet);
  switch (value_table_packet->type) {
  case VALUE_TABLE_TYPE_HISTOGRAM: /* histogram data */
    export_histogram_vtable(datfile, value_table_packet);
    break;
  case VALUE_TABLE_TYPE_TIME_SERIES: /* series of counter data */
    export_time_series_vtable(datfile, personality_info, value_table_packet);
    break;
  case VALUE_TABLE_TYPE_SAMPLES: /* data table of samples */
    export_samples_vtable(datfile, value_table_packet);
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
