/** \file freemcan-device.h
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

/* According to POSIX.1-2001 */
#include <sys/select.h>

/* According to earlier standards */
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "frame-defs.h"

/** Open device */
void dev_init(const char *device_name);

/** Close device */
void dev_fini(void);

/** Set up select() data structure with device data */
int  dev_select_set_in(fd_set *in_fdset, int maxfd);

/** Do device's IO stuff if necessary (from select loop) */
void dev_select_do_io(fd_set *in_fdset);

/** Write a command to the device.
 *
 * \param cmd The #frame_cmd_t to send.
 * \param param The param is only used if cmd is #FRAME_CMD_MEASURE.
 *              Otherwise, it is ignored.
 */
void dev_command(const frame_cmd_t cmd, const uint16_t param);

/** @} */

#endif /* !FREEMCAN_DEVICE_H */
