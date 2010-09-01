/** \file hostware/freemcan-select.h
 * \brief select(2) helpers (interface)
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
 * \defgroup freemcan_select select(2) related definitions/documentation
 * \ingroup mainloop_select
 *
 */


#ifndef FREEMCAN_SELECT_H
#define FREEMCAN_SELECT_H

/* According to POSIX.1-2001 */
#include <sys/select.h>

/* According to earlier standards */
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

/** \addtogroup freemcan_select
 * @{
 */

/** Setup function main loop needs to call in every iteration
 *
 * \param in_fdset Run FD_SET on this
 * \param maxfd    The old maxfd value
 * \return New maxfd, i.e. MAX(maxfd, our_highest_fd_we_set)
 */
typedef int (*select_set_in_t)(fd_set *in_fdset, int maxfd);

/** IO function main loop needs to call in every iteration */
typedef void (*select_do_io_t)(fd_set *in_fdset);

/** @} */

#endif /* !FREEMCAN_SELECT_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
