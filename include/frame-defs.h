/** \file frame-defs.h
 * \brief Data frame definitions
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

/** \section Communication Protocol
 *
 * \subsection Frames sent from hostware to firmware
 *
 * To keep the parser in the firmware simple, all "frames" sent from
 * the hostware to the firmware are actually just a single byte.
 *
 * \subsection Frames sent from firmware to hostware
 *
 * <table>
 *  <tr><th>size in bytes</th> <th>C type define</th> <th>description</th></tr>
 *
 *  <tr><td>4</td> <td>FRAME_MAGIC</td> <td>magic value for beginning of frame</td></tr>
 *  <tr><td>2</td> <td>uint16_t</td> <td>size of payload data in bytes</td></tr>
 *  <tr><td>1</td> <td>frame_type_t</td> <td>frame type</td></tr>
 *  <tr><td>see above</td> <td>?</td> <td>payload data</td></tr>
 *  <tr><td>1</td> <td>uint8_t</td> <td>checksum (Not Implemented Yet)</td></tr>
 * </table>
 *
 */

#ifndef FRAME_DEFS_H
#define FRAME_DEFS_H


#include <stdint.h>


/** Header marker for data frames to host */
#define FRAME_MAGIC  \
  ( \
   (((uint32_t)'F')<<0) |			\
   (((uint32_t)'M')<<8) |			\
   (((uint32_t)'P')<<16) |			\
   (((uint32_t)'K')<<24)			\
    )


/** Data frame types (data frame to host)
 *
 * The values are all upper case ASCII letters.
 */
typedef enum {

  /** text message for debugging */
  FRAME_TYPE_TEXT = 'T',

  /** histogram data */
  FRAME_TYPE_HISTOGRAM = 'H',

  /** device status message */
  FRAME_TYPE_STATUS = 'S'

} frame_type_t;


/** Command frame types (command from host)
 *
 * The values are all lower case ASCII letters.
 */
typedef enum {

  /** Start new measurement */
  FRAME_CMD_MEASURE = 'm',

  /** Transmit intermediate results, then resume measurement */
  FRAME_CMD_INTERMEDIATE = 'i',

  /** Abort running measurement and transmit current results */
  FRAME_CMD_ABORT = 'a',

  /** Reset device */
  FRAME_CMD_RESET = 'r'

} frame_cmd_t;


#endif /* !FRAME_DEFS_H */
