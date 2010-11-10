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
 * \ingroup common
 *
 * These compiler specific definitions are all specific to GCC, but as
 * we are using avr-gcc for the embedded stuff and a native host gcc
 * all our compiles actually are GCC, so that works for us.
 *
 * @{
 */


/** This is defined to have doxygen ignore the packed attribute
 *
 * See also the PREDEFINED definition in Doxyfile.
 */
#define PACKED __attribute__ ((packed))


/** Rename Unused Parameter */
#define _UP(x) unused_p__ ## x

/** Mark Unused Parameter */
#define UP(x) _UP(x) __attribute__((unused))

/** Rename Unused Variable */
#define _UV(x) unused_v__ ## x

/** Mark Unused Variable */
#define UV(x) _UV(x) __attribute__((unused))


/** Compile time assertion, to be used within a function */
#define COMPILE_TIME_ASSERT(CONDITION) \
  switch (0) {            \
  case 0:                 \
  case (CONDITION):       \
    break;                \
  }


/** Helper for BARE_COMPILE_TIME_ASSERT() macro
 *
 * This is ugly C preprocessor macro hacking to get the value of the
 * macro __LINE__ to be used in the function name. You need not
 * understand how this works - just accept that it does.
 */
#define MAKE_BARE_COMPILE_TIME_ASSERT_NAME \
  MAKE_BARE_COMPILE_TIME_ASSERT_NAME1(COMPILE_TIME_ASSERT_fails_in_line, __LINE__)


/** Helper for BARE_COMPILE_TIME_ASSERT() macro
 *
 * This is ugly C preprocessor macro hacking to get the value of the
 * macro __LINE__ to be used in the function name. You need not
 * understand how this works - just accept that it does.
 */
#define MAKE_BARE_COMPILE_TIME_ASSERT_NAME1(BASE, PARAM) \
  MAKE_BARE_COMPILE_TIME_ASSERT_NAME2(BASE, PARAM)


/** Helper for BARE_COMPILE_TIME_ASSERT() macro
 *
 * This is ugly C preprocessor macro hacking to get the value of the
 * macro __LINE__ to be used in the function name. You need not
 * understand how this works - just accept that it does.
 */
#define MAKE_BARE_COMPILE_TIME_ASSERT_NAME2(BASE, PARAM)        \
  BASE ## _ ## PARAM


/** Compile time assertion, to be used outside a function
 *
 * The generated function is generated "naked", so that it does not
 * actually produce any code in the output. Thus such the compile time
 * assertion does not end up producing any actual code - just the
 * symbol table in the object files which is acceptable.
 */
#define BARE_COMPILE_TIME_ASSERT(CONDITION)               \
  void MAKE_BARE_COMPILE_TIME_ASSERT_NAME(void)           \
       __attribute__ ((naked));                           \
  void MAKE_BARE_COMPILE_TIME_ASSERT_NAME(void)           \
  {                                                       \
    COMPILE_TIME_ASSERT(CONDITION);                       \
  }


/** @} */

#endif /* !COMPILER_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
