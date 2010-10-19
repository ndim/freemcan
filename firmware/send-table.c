/** \file firmware/send-table.c
 * \brief Send Table of Measured Data
 *
 * \author Copyright (C) 2010 samplemaker
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
 * \defgroup send_table Send Table of Measured Data
 * \ingroup firmware
 *
 * Send the table of measured data to the host.
 *
 * @{
 */


#include "global.h"
#include "send-table.h"
#include "uart-comm.h"
#include "frame-comm.h"
#include "measurement-timer.h"
#include "data-table.h"


/** \addtogroup firmware_comm
 * @{
 */


#ifdef INVENTED_HISTOGRAM

/** created from binary via objcopy */
extern uint8_t invented_histogram[] asm("_binary_invented_value_table_bin_start") PROGMEM;
/** created from binary via objcopy */
extern uint8_t invented_value_table_size[] asm("_binary_invented_value_table_bin_size") PROGMEM;
/** created from binary via objcopy */
extern uint8_t invented_value_table_end[] asm("_binary_invented_value_table_bin_end") PROGMEM;

#if (ELEMENT_SIZE_IN_BYTES == 3)
/** Simulate a histogram based on the invented histogram data */
static
void invent_histogram(const uint16_t duration)
{
  uint8_t *t8 = (uint8_t *)table;
  for (size_t j=0; j<3*MAX_COUNTER; j+=3) {
    const uint32_t v =
      (((uint32_t)pgm_read_byte(&(invented_histogram[j+0])))<< 0) +
      (((uint32_t)pgm_read_byte(&(invented_histogram[j+1])))<< 8) +
      (((uint32_t)pgm_read_byte(&(invented_histogram[j+2])))<<16);
    const uint32_t r = (v*duration) >> 8;
    t8[j+0] = (r>> 0) & 0xff;
    t8[j+1] = (r>> 8) & 0xff;
    t8[j+2] = (r>>16) & 0xff;
  }
}
#endif

#endif


/** Send value table packet to controller via serial port (layer 3).
 *
 * \param type The reason why we are sending the value table
 *             (#packet_value_table_reason_t).
 *
 * Note that send_table() might take a significant amount of time.
 * For example, at 9600bps, transmitting a good 3KByte will take a
 * good 3 seconds.  If you disable interrupts for that time and want
 * to continue the measurement later, you will want to properly pause
 * the timer.  We are currently keeping interrupts enabled if we
 * continue measuring, which avoids this issue.
 *
 * Note that for 'I' value tables it is possible that we send fluked
 * values due to overflows.
 */
void send_table(const packet_value_table_reason_t reason)
{
  const uint16_t duration = get_duration();

#ifdef INVENTED_HISTOGRAM
  invent_histogram(duration);
#endif

  packet_value_table_header_t header = {
    table_element_size,
    reason,
    value_table_type,
    duration,
    orig_timer_count
  };
  frame_start(FRAME_TYPE_VALUE_TABLE, sizeof(header)+sizeof_data_table);
  uart_putb((const void *)&header, sizeof(header));
  uart_putb((const void *)data_table, sizeof_data_table);
  frame_end();
}


/** @} */


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
