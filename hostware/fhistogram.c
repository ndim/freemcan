/** \file hostware/fhistogram.c
 * \brief Histogram Class (implementation)
 *
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
 * \defgroup fhistogram FHistogram class
 * \ingroup freemcan_gui
 * \bug Needs proper migration from fhistogram_t to GObject based FHistogram
 * @{
 */

#include <assert.h>
#include <stdbool.h>

#include "fhistogram.h"
#include "freemcan-log.h"


static
fhistogram_t *fhistogram_new_uninitialized(const size_t elements)
  __attribute__((warn_unused_result))
  __attribute__((malloc));

static
fhistogram_t *fhistogram_new_uninitialized(const size_t elements)
{
  bool power_of_two_elements = (elements && !(elements & (elements - 1)));
  assert(power_of_two_elements);
  const size_t mem_size =
    sizeof(fhistogram_t) + elements*sizeof(double);
  fmlog("%s(%d) mem_size=%d", __PRETTY_FUNCTION__, elements, mem_size);
  fhistogram_t *result = malloc(mem_size);
  assert(result);
  result->refs = 1;
  result->packet = NULL;
  return result;
}


void fhistogram_ref(fhistogram_t *histogram)
{
  assert(histogram->refs > 0);
  histogram->refs++;
}


static
void fhistogram_free(fhistogram_t *histogram)
{
  packet_histogram_unref(histogram->packet);
  free(histogram);
}


void fhistogram_unref(fhistogram_t *histogram)
{
  assert(histogram->refs > 0);
  histogram->refs--;
  if (histogram->refs == 0) {
    fhistogram_free(histogram);
  }
}


fhistogram_t *fhistogram_new_zero(const size_t elements)
{
  fhistogram_t *result = fhistogram_new_uninitialized(elements);
  packet_histogram_t *packet = packet_histogram_new(0, 0, 1, elements, 0, NULL);
  assert(packet);
  result->packet = packet;
  assert(packet->element_count <= 4096);
  return result;
}


fhistogram_t *fhistogram_new_from_packet(packet_histogram_t *packet)
{
  assert(packet);
  fmlog("%s(%p) packet->element_count=%d",
	__PRETTY_FUNCTION__, (void *)packet, packet->element_count);

  fhistogram_t *hist = fhistogram_new_uninitialized(packet->element_count);
  hist->packet = packet;
  packet_histogram_ref(hist->packet);
  assert(hist->packet->element_count <= 4096);
  return hist;
}


/* @} */
