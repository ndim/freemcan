/** \file hostware/freemcan-tui.h
 * \brief Freemcan interactive text user interface (non-ncurses)
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
 * \addtogroup hostware_tui
 *
 * @{
 */

#ifndef FREEMCAN_TUI_H
#define FREEMCAN_TUI_H

#include <stdbool.h>

bool quit_flag;

void tui_init();
void tui_fini();
void tui_do_io(void);
const char *main_init(int argc, char *argv[]);


typedef void (*send_command_f)(const frame_cmd_t cmd, const uint16_t param);
extern send_command_f tui_device_send_command;


#endif /* !FREEMCAN_TUI_H */

/** @} */