/** \file hostware/freemcan-gui-device.h
 * \brief Freemcan GUI device event hookup (interface)
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
 */

#ifndef FREEMCAN_GUI_DEVICE_H
#define FREEMCAN_GUI_DEVICE_H

#include <stdint.h>
#include <glib.h>

#include "frame-defs.h"

/** \addtogroup freemcan_gui_device
 * @{
 */

GSource *device_source_new(void);

gboolean gui_device_init_func(gpointer data);

extern gchar *init_device_fname;

void gui_device_command(const frame_cmd_t cmd, const uint16_t param);

/** @} */

#endif /* !FREEMCAN_GUI_DEVICE_H */
