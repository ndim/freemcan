/** \file hostware/freemcan-signals.c
 * \brief Common text user interface related signal functions (implementation)
 * \ingroup hostware_tui
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
 * \defgroup freemcan_signals Signal Handling (probably TUI specific)
 * \ingroup hostware_tui
 *
 * @{
 */


#include <assert.h>
#include <signal.h>
#include <stdio.h>

#include <stdarg.h>
#include <errno.h>
#include <string.h>

#include "compiler.h"

#include "freemcan-log.h"
#include "freemcan-signals.h"


/** flag set by SIGINT handler */
bool sigint = false;


/** our SIGINT handler */
void sigint_handler(int UP(i))
{
  sigint = true;
}


/** flag set by SIGTERM handler */
bool sigterm = false;


/** our SIGTERM handler */
void sigterm_handler(int UP(i))
{
  sigterm = true;
}


/** set up our special signal handling */
static void signals_init(void);


/** set up our special signal handling */
static void signals_init(void)
{
  sighandler_t UV(int_handler)  = signal(SIGINT,  sigint_handler);
  sighandler_t UV(term_handler) = signal(SIGTERM, sigterm_handler);
  /* linux will try and restart an interrupted system call by default */
  siginterrupt(SIGINT,  1); /* stop system calls on SIGINT */
  siginterrupt(SIGTERM, 1); /* stop system calls on SIGTERM */
}


static void common_init(void) __attribute__((constructor));
static void common_init(void)
{
  /* fmlog("freemcan-signals.c: common_init()"); */
  signals_init();
}


static void common_done(void) __attribute__((destructor));
static void common_done(void)
{
  /* fmlog("freemcan-signals.c: common_done()"); */
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
