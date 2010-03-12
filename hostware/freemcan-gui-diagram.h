/** \file hostware/freemcan-gui-diagram.h
 * \brief Freemcan GUI diagram drawing and related things (interface)
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
 */

#ifndef FREEMCAN_GUI_DIAGRAM_H
#define FREEMCAN_GUI_DIAGRAM_H

#include <time.h>
#include <cairo/cairo.h>

#include "fhistogram.h"
#include "freemcan-packet.h"


/**
 * \addtogroup freemcan_gui_diagram
 * @{
 */

void draw_diagram(cairo_t *cr, const double w, const double h,
		  const fhistogram_t *histogram)
  __attribute__((nonnull(1,4)));

void draw_diagram_to_png_file(const fhistogram_t *histogram)
  __attribute__((nonnull(1)));
void draw_diagram_to_pdf_file(const fhistogram_t *histogram)
  __attribute__((nonnull(1)));
void draw_diagram_to_eps_file(const fhistogram_t *histogram)
  __attribute__((nonnull(1)));
void draw_diagram_to_svg_file(const fhistogram_t *histogram)
  __attribute__((nonnull(1)));

/** @} */

#endif /* !FREEMCAN_GUI_DIAGRAM_H */

