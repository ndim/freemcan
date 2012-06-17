/** \file firmware/init-functions.h
 * \brief Init functions run at system startup before main()
 *
 * \author Copyright (C) 2011 samplemaker
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
 * \addtogroup init_functions
 * @{
 */

#ifndef INIT_FUNCTIONS_H
#define INIT_FUNCTIONS_H


/** Declare an init function to be run on processor startup before main().
 *
 * The function name is not globally visible ("static"), but you
 * should still give it a useful name so that you can see in listings
 * and other files where the code comes from.
 *
 * @param SECTION  Name of the linker section to place the code in
 * @param FUN_NAME Name of the initialization function
 *
 * Example Usage:
 *
 * \code
 *     INIT_FUNCTION(init5, switch_init) {
 *         DDRB  &= ~_BV(DDB2);
 *         PORTB |=  _BV(PB2);
 *     }
 * \endcode
 *
 */
#define INIT_FUNCTION(SECTION, FUN_NAME)	\
  static void FUN_NAME(void)			\
       __attribute__ ((naked))			\
       __attribute__ ((used))			\
       __attribute__ ((section("." #SECTION))); \
  static void FUN_NAME(void)


#endif /* !INIT_FUNCTIONS_H */


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
