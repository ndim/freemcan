/** \file firmware/timer2-debounce-switch.h
 * \brief Debounce switch using timer2
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
 * \addtogroup timer2_debounce_switch
 * @{
 */


#ifndef TIMER2_DEBOUNCE_SWITCH
#define TIMER2_DEBOUNCE_SWITCH


#include <stdint.h>


/** Flag showing whether the switch is activated or note.
 *
 * Interpret this as a boolean value:
 *   0x0000   - switch is active
 *   non-zero - switch is inactive
 *
 * We use this strange kind of logic for easier internal implementation.
 */
extern uint16_t switch_is_inactive;


#endif /* !TIMER2_DEBOUNCE_SWITCH */


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
