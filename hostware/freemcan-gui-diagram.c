/** \file hostware/freemcan-gui-diagram.c
 * \brief Freemcan GUI diagram drawing and related things (implementation)
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
 * \defgroup freemcan_gui_diagram Diagram Drawing
 * \ingroup freemcan_gui
 * @{
 */

#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <cairo/cairo.h>
#include <cairo/cairo-pdf.h>
#include <cairo/cairo-ps.h>
#include <cairo/cairo-svg.h>

#include "compiler.h"

#include "freemcan-export.h"
#include "freemcan-log.h"

#include "freemcan-gui-diagram.h"


static
double gauss(const double sigma, const double m, const double x)
{
  const double fact = 1.0 / (sigma*sqrt(2*M_PI));
  const double nexp = (x-m)/sigma;
  const double expo = -0.5 * nexp * nexp;
  const double result = (fact * exp(expo));
  return result;
}


static
fhistogram_t *invented_histogram = NULL;


/** Invent some histogram data */
static void invent_histogram()
{
  if (invented_histogram) {
    return;
  }

  invented_histogram = fhistogram_new_zero(512);

  for (size_t i=0; i<invented_histogram->packet->element_count; i++) {
    const double di = i;
    const double rnum = rand();
    const double rval = 0.5*(-0.5+rnum/RAND_MAX);
    const double val = rval + 200*gauss(15, 60, di) + 80*gauss(25, 210, di);
    const double aval = fabs(val);
    invented_histogram->packet->elements[i] = aval;
    if (aval > invented_histogram->packet->max_value) {
      invented_histogram->packet->max_value = aval;
    }
  }
}


double draw_diagram_text(cairo_t *cr, const double x, const double y,
			 const char *text)
{
  cairo_text_extents_t extents;
  cairo_set_font_size(cr, 16);
  cairo_text_extents(cr, text, &extents);
  cairo_move_to(cr, x-extents.width, y+extents.height);
  cairo_set_source_rgb(cr, 0.0, 0.0, 0.0);
  cairo_show_text(cr, text);
  return (y+extents.height+2);
}


void draw_diagram(cairo_t *cr, const double w, const double h,
		  const fhistogram_t *parm_histogram)
{
  invent_histogram();
  const fhistogram_t *hist = parm_histogram?parm_histogram:invented_histogram;
  /* Paint histogram */
  const double hist_max   = hist->packet->max_value;
  const size_t hist_count = hist->packet->element_count;

  fmlog("%s: hist=%p, hist_count=%d, hist_max=%f", __PRETTY_FUNCTION__,
	(void *)hist, hist_count, hist_max);

  char datestr[128];
  const struct tm *tm_ = localtime(&hist->packet->receive_time);
  if (hist->packet->receive_time && tm_) {
    strftime(datestr, sizeof(datestr), "%Y-%m-%d.%H:%M:%S", tm_);
  } else {
    strcpy(datestr, "timestamp ???");
  }
  const double x0 = w-3;
  const double y0 = 3;
  const double y1 = draw_diagram_text(cr, x0, y0, datestr);
  const char *unknown = "type ???";
  char known[] = "type '?'";
  const char *foo = NULL;
  const packet_histogram_type_t type = hist->packet->type;
  switch (type) {
  case PACKET_HISTOGRAM_ABORTED:      foo = "result of aborted measurement"; break;
  case PACKET_HISTOGRAM_DONE:         foo = "result of finished measurement"; break;
  case PACKET_HISTOGRAM_INTERMEDIATE: foo = "intermediate result"; break;
  case PACKET_HISTOGRAM_RESEND:       foo = "resent finished measurement result"; break;
  }
  if (!foo) {
    if ((32 <= type) && (type < 127)) {
      known[6] = type;
      foo = known;
    } else {
      foo = unknown;
    }
  }
  const double y2 = draw_diagram_text(cr, x0, y1, foo);
  char buf[128];

  /* http://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2 */
  const size_t v = hist_count;
  const bool is_power_of_two = v && !(v & (v - 1));
  if (is_power_of_two) {
    int bits = 0;
    size_t v = hist_count;
    /* http://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightLinear */
    if (v) {
      v = (v ^ (v-1)) >> 1;
      for (bits = 0; v; bits++) {
	v >>= 1;
      }
    }
    snprintf(buf, sizeof(buf), "ADC resolution: %d bits", bits);
  } else {
    strcpy(buf, "ADC resolution error");
  }
  const double y3 = draw_diagram_text(cr, x0, y2, buf);
  snprintf(buf, sizeof(buf),
	   "Measurement duration: %d s", hist->packet->duration);
  const double UV(y4) = draw_diagram_text(cr, x0, y3, buf);

  if (hist == invented_histogram) {
    cairo_set_source_rgba(cr, 0.3, 0.3, 0.3, 0.5);
  } else {
    cairo_set_source_rgb(cr, 0.0, 0.0, 0.8);
  }

  fmlog("%s: hist=%p hist->elements=%p", __PRETTY_FUNCTION__,
	(void*)hist, (void*)hist->packet->elements);

  const double bw = w/hist_count;
  const double bhfact = h/hist_max;
  for (size_t i=0; i<hist_count; i++) {
    const double val = hist->packet->elements[i];
    const double bh = bhfact*val;
    cairo_rectangle(cr, i*bw, h-bh, bw, bh);
    cairo_fill(cr);
  }
}


static
void draw_diagram_to_file(cairo_t *cr, const double w, const double h,
			  const fhistogram_t *histogram)
{
  /* background */
  if (1) {
    cairo_rectangle(cr, 0.0, 0.0, w, h);
    cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
    cairo_fill(cr);
  }

  /* draw background text */
  if (1) {
    const char *text = "FreeMCAn";
    cairo_text_extents_t extents;
    cairo_set_font_size(cr, 0.2*h);
    cairo_text_extents(cr, text, &extents);
    cairo_move_to(cr, 0.5*(w-extents.width), 0.5*(0.3*h+extents.height));
    cairo_set_source_rgba(cr, 0.5, 0.0, 0.0, 0.1);
    cairo_show_text(cr, text);
  }

  /* draw actual diagram */
  draw_diagram(cr, w, h, histogram);
}


/** Diagram file width (in points) */
#define FILE_WIDTH_IN_PT (800)

/** Diagram file height (in points) */
#define FILE_HEIGHT_IN_PT (500)

/** Diagram file width (in pixels) */
#define FILE_WIDTH_IN_PX (1024)

/** Diagram file height (in pixels) */
#define FILE_HEIGHT_IN_PX (600)


void draw_diagram_to_png_file(const fhistogram_t *histogram)
{
  const char *filename = export_histogram_get_filename(histogram->packet, "png");
  const int w = FILE_WIDTH_IN_PX;
  const int h = FILE_HEIGHT_IN_PX;
  cairo_surface_t *surf = cairo_image_surface_create(CAIRO_FORMAT_RGB24, w, h);
  cairo_t *cr = cairo_create(surf);

  draw_diagram_to_file(cr, w, h, histogram);

  cairo_surface_write_to_png(surf, filename);
  cairo_surface_destroy(surf);
}


void draw_diagram_to_pdf_file(const fhistogram_t *histogram)
{
  const char *filename = export_histogram_get_filename(histogram->packet, "pdf");
  const int w = FILE_WIDTH_IN_PT;
  const int h = FILE_HEIGHT_IN_PT;
  cairo_surface_t *surf = cairo_pdf_surface_create(filename, w, h);
  cairo_t *cr = cairo_create(surf);

  draw_diagram_to_file(cr, w, h, histogram);

  cairo_surface_flush(surf);
  cairo_surface_finish(surf);
  cairo_surface_destroy(surf);
}


void draw_diagram_to_eps_file(const fhistogram_t *histogram)
{
  const char *filename = export_histogram_get_filename(histogram->packet, "eps");
  const int w = FILE_WIDTH_IN_PT;
  const int h = FILE_HEIGHT_IN_PT;
  cairo_surface_t *surf = cairo_ps_surface_create(filename, w, h);
  cairo_ps_surface_set_eps(surf, true);
  cairo_ps_surface_dsc_comment (surf, "%%Title: FreeMCAn diagram");

  cairo_t *cr = cairo_create(surf);

  draw_diagram_to_file(cr, w, h, histogram);

  cairo_surface_flush(surf);
  cairo_surface_finish(surf);
  cairo_surface_destroy(surf);
}


void draw_diagram_to_svg_file(const fhistogram_t *histogram)
{
  const char *filename = export_histogram_get_filename(histogram->packet, "svg");
  const int w = FILE_WIDTH_IN_PX;
  const int h = FILE_HEIGHT_IN_PX;
  cairo_surface_t *surf = cairo_svg_surface_create(filename, w, h);
  cairo_t *cr = cairo_create(surf);

  draw_diagram_to_file(cr, w, h, histogram);

  cairo_surface_flush(surf);
  cairo_surface_finish(surf);
  cairo_surface_destroy(surf);
}


/** @} */
