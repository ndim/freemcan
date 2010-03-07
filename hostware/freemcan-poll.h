/** \file hostware/freemcan-poll.h
 * \brief poll(2) helpers (interface)
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
 * \defgroup freemcan_poll poll(2) related definitions/documentation
 * \ingroup mainloop_poll
 *
 */

#ifndef FREEMCAN_POLL_H
#define FREEMCAN_POLL_H

#include <poll.h>

/** \addtogroup freemcan_poll
 * @{
 */

/** Event handler function called from main loop */
typedef void (*poll_handler_t)(struct pollfd *pfd);

/** The common function everyone needs to implement, and main() needs to call */
typedef void (*poll_setup_t)(struct pollfd *pollfds, poll_handler_t *pollhandlers,
			     nfds_t *index, const nfds_t limit);

#endif /* !FREEMCAN_POLL_H */

/** @} */
