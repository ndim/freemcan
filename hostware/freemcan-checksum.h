/** \file freemcan-checksum.h
 * \brief Checksum for layer 2 frames (interface)
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
 * \addtogroup freemcan_frame_checksum
 * @{
 */


#ifndef FREEMCAN_CHECKSUM_H
#define FREEMCAN_CHECKSUM_H

#include <stdbool.h>
#include <stdlib.h>

#include "frame-defs.h"


/** Reset checksum state machine */
void checksum_reset(void);

/** Update checksum state machine with value */
void checksum_update(const uint8_t value);

/** Write checksum to file descriptor */
void checksum_write(const int fd);

/** Match value against internal checksum state */
bool checksum_match(const uint8_t value);


/** @} */

#endif /* !FREEMCAN_CHECKSUM_H */
