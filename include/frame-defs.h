/** \file include/frame-defs.h
 * \brief Data frame definitions (layer 2)
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
 * \defgroup communication_protocol Communication Protocol
 * \ingroup common
 *
 * \section embedded_fsm Firmware state machine
 *
 * The firmware implements the following state machine:
 *
 * \image html firmware-states.png "Device as State Machine"
 *
 * The input triggering a state transition is either a byte received
 * over the serial line, or a timeout happening (watchdog timeout, or
 * measurement duration has passed).
 *
 * Note1: The "booting" state is a hardware state. The others are
 *        software states.
 *
 * Note2: Entering an upper case state is always reported by a state
 *        packet.
 *
 * Note3: The bold black edges show the default way through the
 *        state transition diagram.
 *
 * Note4: The red edge are error conditions.
 *
 * Note5: The green edges show the actions of the optional 's' (state)
 *        command which just sends the current state and resumes
 *        whatever it was doing at the time.
 *
 * Note6: The blue edges are reactions to the other optional commands.
 *
 * \section layer_model Layering model
 *
 * To keep the parser in the firmware simple, we use a simpler data
 * format for communication sent from the host to the device (\ref
 * frame_host_to_emb). For the more complex data sent from the device
 * to the hostware, we use the following layering model:
 *
 * <table class="table header-top header-left">
 *  <tr><th>layer</th><th>description</th><th>specification</th>
 *      <th>hostware implementation</th><th>firmware implementation</th></tr>
 *  <tr><th>4</th><td>application layer (process the packets' content)</td>
 *      <td>N/A</td><td>\ref tui_data_handling</td><td>\ref firmware</td></tr>
 *  <tr><th>3</th><td>packets of a certain type with a certain content</td>
 *      <td>\ref packet_defs</td><td>\ref freemcan_packet_parser</td><td>\ref firmware_comm</td></tr>
 *  <tr><th>2</th><td>frames of a certain size</td>
 *      <td>\ref frame_emb_to_host</td><td>\ref freemcan_frame_parser</td><td>\ref frame_comm</td></tr>
 *  <tr><th>1</th><td>byte stream to/from serial port</td>
 *      <td>\ref uart_defs</td><td>\ref freemcan_device</td><td>\ref uart_comm</td></tr>
 *  <tr><th>0</th><td>physical: bits on the wire between serial ports</td>
 *      <td>N/A</td><td>N/A</td><td>N/A</td></tr>
 * </table>
 *
 * \section endianness Endianness
 * In the communication between firmware and hostware, all values
 * larger than a single byte are defined to be little endian. The
 * reason is that we want to avoid all unnecessary work in the
 * firmware (like endianness conversion) and avr-gcc provides us a
 * with little endian system.
 *
 * \defgroup frame_defs Frame Format
 * \ingroup communication_protocol
 * @{
 *
 * \section frame_protocol Frame Communication Protocol (Layer 2)
 *
 * \subsection frame_host_to_emb Frames sent from hostware to firmware
 *
 * To keep the parser in the firmware simple, most "frames" sent from
 * the hostware to the firmware are actually just a single byte:
 *
 * <table class="table header-top">
 *  <tr><th>size in bytes</th> <th>C type define</th> <th>description</th></tr>
 *  <tr><td>1</td> <td>#frame_cmd_t</td> <td>frame command type</td></tr>
 * </table>
 *
 * The single exception is the "start measurement" command which looks
 * as follows:
 *
 * <table class="table header-top">
 *  <tr><th>size in bytes</th> <th>value</th> <th>C type define</th> <th>description</th></tr>
 *  <tr><td>1</td> <td>FRAME_CMD_MEASURE</td> <td>#frame_cmd_t</td> <td>frame command type</td></tr>
 *  <tr><td>2</td> <td>?</td> <td>uint16_t</td> <td>timervalue (measurement duration)</td></tr>
 *  <tr><td>1</td> <td>checksum</td> <td>uint8_t</td> <td>checksum over the last three bytes</td></tr>
 * </table>
 *
 * \subsection frame_emb_to_host Frames sent from firmware to hostware
 *
 * <table class="table header-top">
 *  <tr><th>size in bytes</th> <th>C type define</th> <th>description</th></tr>
 *
 *  <tr><td>4</td> <td>#FRAME_MAGIC_LE_U32<br>or #FRAME_MAGIC_STR</td> <td>magic value marking beginning of frame</td></tr>
 *  <tr><td>2</td> <td>uint16_t</td> <td>size of payload data in bytes</td></tr>
 *  <tr><td>1</td> <td>#frame_type_t</td> <td>frame type</td></tr>
 *  <tr><td>see above</td> <td>?</td> <td>payload data</td></tr>
 *  <tr><td>1</td> <td>uint8_t</td> <td>checksum</td></tr>
 * </table>
 *
 * \todo Document checksum algorithm.
 *
 */

#ifndef FRAME_DEFS_H
#define FRAME_DEFS_H


#include <stdint.h>


/** Header magic marker value for data frames to host, AVR uint32_t version.
 *
 * This is good for the little endian AVR controller.
 */
#define FRAME_MAGIC_LE_U32  \
  ( \
   (((uint32_t)'F')<<0) |			\
   (((uint32_t)'M')<<8) |			\
   (((uint32_t)'P')<<16) |			\
   (((uint32_t)'K')<<24)			\
    )

/** Header magic marker value for data frames to host, string version.
 *
 * This is good for endianness independent char-by-char receivers.
 */
#define FRAME_MAGIC_STR "FMPK"

/** Data frame types (data frame to host)
 *
 * The values are all upper case ASCII letters.
 */
typedef enum {

  /** text message for reporting and debugging */
  FRAME_TYPE_TEXT = 'T',

  /** histogram data */
  FRAME_TYPE_HISTOGRAM = 'H',

  /** device state message */
  FRAME_TYPE_STATE = 'S'

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

  /** Query current state */
  FRAME_CMD_STATE = 's',

  /** Reset device */
  FRAME_CMD_RESET = 'r'

} frame_cmd_t;

/** @} */

#endif /* !FRAME_DEFS_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
