/** \file firmware/wdt-softreset.h
 * \brief Reset the AVR processor via the watchdog timer
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
 * \addtogroup wdt_softreset
 * @{
 */

#ifndef WDT_SOFTRESET_H
#define WDT_SOFTRESET_H


#include <avr/wdt.h>


/** Trigger AVR reset via watchdog device. */
static inline
void wdt_soft_reset(void)
  __attribute__((noreturn));
static inline
void wdt_soft_reset(void)
{
  do {
    wdt_enable(WDTO_15MS);
    while (1) {
      /* wait until watchdog has caused a system reset */
    }
  } while(0);
}


/** @} */

#endif /* WDT_SOFTRESET_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
