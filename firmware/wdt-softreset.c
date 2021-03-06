/** \file firmware/wdt-softreset.c
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
 * \defgroup wdt_softreset AVR Reset via watchdog timer
 * \ingroup firmware_generic
 *
 * Reset the AVR processor via the watchdog timer.
 *
 * @{
 */


#include "wdt-softreset.h"
#include "init-functions.h"


/** Disable watchdog on device reset.
 *
 * Newer AVRs do not disable the watchdog on reset, so we need to
 * disable it manually early in the startup sequence. "Newer" AVRs
 * include the 164P/324P/644P we are using.
 *
 * \bug We need to reset WDTCSR here as well. The
 *      ATmega644P/ATmega1284P datasheets suggest the WDT might
 *      accidentally be turned on by a brownout, and we should deal
 *      with that. avr-libc's wdt_disable() does not do all that the
 *      datasheet suggests. We can leave in MCUSR=0, though, as we are
 *      not concerned with the reset cause.
 *
 * See http://www.nongnu.org/avr-libc/user-manual/FAQ.html#faq_softreset
 */
INIT_FUNCTION(init3, wdt_softreset_init)
{
  MCUSR = 0;
  wdt_disable();
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
