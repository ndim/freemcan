/** \file hostware/freemcan-log.h
 * \brief Logging mechanism interface
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
 * \addtogroup freemcan_log
 * @{
 */


#ifndef FREEMCAN_LOG_H
#define FREEMCAN_LOG_H

#include <stdint.h>
#include <stdlib.h>

/** Write a log message somewhere
 * \param data Private data for the log handler function
 * \param message The message string (nul-terminated)
 * \param length Length of the message in characters.
 * \return ssize_t A value >= in case of success, a value < 0 in
 * case of error.
 */
typedef void (*fmlog_handler_t)(void *data,
                                const char *message,
                                const size_t length);

/** Reset fmlog message handler to the default handler */
void fmlog_reset_handler(void);

/** Set the fmlog message handler to the given handler function */
void fmlog_set_handler(fmlog_handler_t the_fmlog_handler, void *the_data);

/** Log a message */
void fmlog(const char *format, ...)
  __attribute__(( format(printf,1,2),
                  nonnull(1) ));

/** Log a message with strerror(errno) */
void fmlog_error(const char *format, ...)
  __attribute__(( format(printf,1,2),
                  nonnull(1) ));

/** Log a block of data as bytes */
void fmlog_data(const char *prefix, const void *data, const size_t size);

/** Log a block of data as 16 bit integers */
void fmlog_data16(const char *prefix, const void *data, const size_t size);

/** Log a block of data as 16 bit integers */
void fmlog_data24(const char *prefix, const void *data, const size_t size);

/** Log a block of data as 32 bit integers */
void fmlog_data32(const char *prefix, const void *data, const size_t size);

/** Log value table data */
void fmlog_value_table(const char *prefix, const uint32_t *elements, const size_t count);

/** @} */

#endif


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
