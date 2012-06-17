/** \file firmware/switch.c
 * \brief Providing a hardware button switch to start a measurement
 *
 * \author Copyright (C) 2011 samplemaker
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
 * \author Copyright (C) 1998, 1999, 2000, 2007, 2008, 2009 Free Software Foundation, Inc.
 *         (for the assembly code in ts_init() to clear data_table)
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
 * \defgroup switch
 * \ingroup firmware_personality_groups
 *
 * The idea here is to make switch use a one-off thing: Once the user
 * has pressed the button, eventually switch_lock() is called and the
 * switch then permanently disabled (in switch OFF state) afterwards.
 *
 * This avoids the need to debounce the switch, as it is only used
 * once and we can use the first bounce for that. The next use of the
 * switch will only happen after a reboot, and thus a long time away,
 * and debouncing stops being an issue.
 *
 * @{
 */

#include "switch.h"

#include <avr/io.h>


/** Initialize peripherals necessary for "start measurement hardware button"
 *
 */
void switch_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void switch_init(void)
{
  DDRB &= ~_BV(DDB2);
  /* enable pull up in order to prevent activating a measurement
     in case the hardware pin is floating (ie using pollin board) */
  PORTB |= _BV(PB2);
}


/** Type def for whether the switch is locked in OFF state */
typedef enum {
  SWITCH_UNLOCKED,
  SWITCH_LOCKED_OFF
} switch_lock_t;


/** Whether the switch is locked in OFF state.
 *
 * Stored as a single uint8_t value by avr-gcc.
 */
static switch_lock_t the_switch_lock = SWITCH_UNLOCKED;


/** Acquire "start of measurement command by hardware button"
 *
 *  Function returns always 0 if the switch was locked by
 *  switch_lock(). Returns elsewise the current hardware switch
 *  status.
 */
uint8_t switch_trigger_measurement(void){
  switch (the_switch_lock) {
  case SWITCH_UNLOCKED:
    return (bit_is_clear(PINB, PB2));
  default:
    return 0;
  }
}


/** Lock switch
 *
 *  Once the switch is locked the measurement cannot be started
 *  ever again except by power on reset or WDT reset
 */
void switch_lock(void){
  the_switch_lock = SWITCH_LOCKED_OFF;
}
