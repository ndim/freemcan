/** \file firmware/global.h
 * \brief Global adjustments for freemcan firmware
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
 * \defgroup global_constants Global Constants And Definitions
 * \ingroup firmware_generic
 * @{
 */

#ifndef GLOBAL_H
#define GLOBAL_H


#ifndef __ASSEMBLER__


/**
 * \defgroup global_flags Global Flags
 * \ingroup firmware
 *
 * The ATmega devices can easily access IO registers below address
 * 0x1F with just 2 bytes of code for setting, clearing, or testing a
 * single bit. Also, those accesses all are atomic.
 *
 * So we try to store all our global flags in GPIOR0 which is an IO
 * register for just that purpose.
 *
 * @{
 */


/** IO register to use for global flags */
#define GLOBAL_FLAG_REGISTER GPIOR0


/** Global flag: Signal that measurement has finished
 *
 * Sends a signal from personality specific ISR code to the main event
 * loop that the measurement has finished.
 *
 * Set only in the ISR code, cleared only in the main event loop.
 *
 * Usage:
 *
 *  0. Startup default value is GF_CLEAR.
 *
 *  1. main event loop starts a measurement
 *
 *  2. personality specific code (ISR) code determines end of
 *     measurement and GF_SETs.
 *
 *  3. main event loop detects GF_ISSET, then handles the event, then
 *     GF_CLEARs.
 */
#define GF_MEASUREMENT_FINISHED 0


/** Is given global flag set? */
#define GF_ISSET(FLAG)                          \
  (0 != ((GLOBAL_FLAG_REGISTER) & _BV(FLAG)))


/** Is given global flag cleared? */
#define GF_IS_CLEARED(FLAG)                     \
  (0 == ((GLOBAL_FLAG_REGISTER) & _BV(FLAG)))


/** Clear given global flag */
#define GF_CLEAR(FLAG)                          \
  do {                                          \
    GLOBAL_FLAG_REGISTER &= ~(_BV(FLAG));       \
  } while (0)


/** Set given global flag */
#define GF_SET(FLAG) \
  do {                                          \
    GLOBAL_FLAG_REGISTER |= _BV(FLAG);          \
  } while (0)


/** @} */


/** Convenience macro for composing bit fields in registers
 *
 * \param VALUE   A value like e.g. 5. Can be a macro.
 * \param BITNAME Bit base name, e.g. WGM0 for the WGM0x bits.
 * \param BITNO   Bit number, e.g. 2 for the WGM02 bit.
 *                Must not be a macro.
 *
 * Example usage to set the WGM20 bit:
 *    BITF(TIMER2_WGM_VALUE, WGM2, 0)
 */
#define BITF(VALUE, BITNAME, BITNO)			\
  ((((VALUE)>>BITNO)&1) * (1<<(BITNAME##BITNO)))


#else /* ifdef __ASSEMBLER__ */


#endif /* __ASSEMBLER__ */


#endif /* !GLOBAL_H */


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
