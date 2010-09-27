/** \file firmware/firmware-version.c
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
 * \ingroup firmware
 *
 * Send the software version string on firmware bootup. If the source
 * tree the firmware was not built from is not a git tree, the printed
 * version will be a static default string.
 *
 * @{
 */


#include <avr/pgmspace.h>


#include "firmware-version.h"
#include "packet-comm.h"
#include "git-version.h"


/** Static constant version string.
 *
 * We do not store this in program space as that would require us to
 * include more code which whould in the end cause even more bytes to
 * be put into the firmware image than we could save by putting this
 * into program space.
 */
static const char version_string[] = "freemcan " GIT_VERSION;


void send_version(void)
{
  send_text(version_string);
}

/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
