/** \file include/compiler.h
 * \brief Compiler Specific Definitions
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
 */

#ifndef COMPILER_H
#define COMPILER_H


/** \defgroup compiler_defs Compiler Specific Definitions
 *
 * These compiler specific definitions are all specific to GCC, but as
 * we are using avr-gcc for the embedded stuff and a native host gcc
 * all our compiles actually are GCC, so that works for us.
 *
 * @{
 */

/** Rename Unused Parameter */
#define _UP(x) unused_p__ ## x

/** Mark Unused Parameter */
#define UP(x) _UP(x) __attribute__((unused))

/** Rename Unused Variable */
#define _UV(x) unused_v__ ## x

/** Mark Unused Variable */
#define UV(x) _UV(x) __attribute__((unused))

/** @} */

#endif /* !COMPILER_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
