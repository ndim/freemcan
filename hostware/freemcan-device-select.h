/** \file hostware/freemcan-device-select.h
 * \brief FreeMCAn device select(2) support (interface)
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


/* According to POSIX.1-2001 */
#include <sys/select.h>

/* According to earlier standards */
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>


/** \addtogroup freemcan_device_select
 * @{
 */

/** Set up select(2) data structure with device data */
int  device_select_set_in(fd_set *in_fdset, int maxfd);

/** Do device's IO stuff if necessary (from select(2) loop) */
void device_select_do_io(fd_set *in_fdset);


void device_select_init(const char *device_name);
void device_select_fini();

/** @} */
