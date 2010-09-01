/** \file hostware/freemcan-device.h
 * \brief FreeMCAn device (interface)
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
 * \addtogroup freemcan_device
 * @{
 */


#ifndef FREEMCAN_DEVICE_H
#define FREEMCAN_DEVICE_H

#include "frame-defs.h"


/** Opaque device type */
struct _device_t;

/** Opaque device type */
typedef struct _device_t device_t;


#include "frame-parser.h"


/** New device */
device_t *device_new(frame_parser_t *frame_parser)
  __attribute__(( nonnull(1) ));


void device_ref(device_t *self)
  __attribute__(( nonnull(1) ));


void device_unref(device_t *self)
  __attribute__(( nonnull(1) ));


/** Open device */
void device_open(device_t *self, const char *device_name)
  __attribute__(( nonnull(1,2) ));


/** Close device */
void device_close(device_t *self)
  __attribute__(( nonnull(1) ));


/** Get device file descriptor */
int device_get_fd(device_t *self)
  __attribute__(( nonnull(1) ));


/** Write a command to the device.
 *
 * \param self The device object
 * \param cmd The #frame_cmd_t to send.
 * \param param The param is only used if cmd is #FRAME_CMD_MEASURE.
 *              Otherwise, it is ignored.
 */
void device_send_command(device_t *self,
                         const frame_cmd_t cmd, const uint16_t param)
  __attribute__(( nonnull(1) ));


/** Do the actual IO
 *
 * Can be called from either the select(2) or poll(2) based main loop
 * hook functions (#device_select_do_io).
 */
void device_do_io(device_t *self)
  __attribute__(( nonnull(1) ));


/** @} */

#endif /* !FREEMCAN_DEVICE_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
