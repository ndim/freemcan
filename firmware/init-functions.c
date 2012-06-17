/** \file firmware/init-functions.c
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
 * \defgroup init_functions Initialization Functions (before main())
 * \ingroup firmware_generic
 *
 * The idea here is to provide a mechanism for generic initialization
 * code to be run at startup before main() actually runs.
 *
 * On Atmega processors, we can just place the code into the startup
 * code via the .initN sections.
 *
 * On other architectures, this may work by placing pointers to the
 * functions into a special section and then iterating over those
 * function pointers (as the Linux kernel does, see
 * http://www.embedded-bits.co.uk/2008/init-call-mechanism/ ).
 *
 * @{
 */


#include "init-functions.h"


/* No implementation code needed for AVR */


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
