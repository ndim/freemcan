/** \file firmware/software-version.c
 * \brief Firmware version string output
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
 * \defgroup software_version Software version string
 * \ingroup firmware_generic
 *
 * Send the software version string on firmware bootup. If the source
 * tree the firmware was not built from is not a git tree, the printed
 * version will be a static default string.
 *
 * @{
 */


#include <avr/pgmspace.h>


#include "packet-comm.h"
#include "git-version.h"


void software_version_send(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init8")));
void software_version_send(void)
{
  send_text_P(PSTR("freemcan " GIT_VERSION));
}

/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
