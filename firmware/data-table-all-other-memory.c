/** \file firmware/data-table-all-other-memory.c
 * \brief Data table occupying all the rest of the SRAM
 *
 * \author Copyright (C) 2010 samplemaker
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
 * \author Copyright (C) 1998, 1999, 2000, 2007, 2008, 2009 Free Software Foundation, Inc.
 *         (for the assembly code in ts_init() to clear data_table)
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
 * \defgroup data_table_all_other_memory Data table occupying all the rest of the SRAM
 * \ingroup firmware_personality_groups
 *
 * Data table occupying all the rest of the SRAM (in conjunction with
 * linker script data-table-all-other-memory.x).
 *
 * @{
 */


#include "init-functions.h"


/* import the data table symbols in data type inspecific way */
extern volatile char data_table[];
extern volatile char data_table_end[];

/* We do not need data_table_size here as we just clear the memory
 * between data_table and data_table_end.
 */

/** data_table initialization code (needs to be run once on startup) */
INIT_FUNCTION(init5, data_table_init)
{
  /** As data_table is outside of the memory area with the normal data
   * (and cannot be easily added to that memory area without
   * essentially completely rewriting the linker script), the
   * data_table content will NOT be cleared by the default avr-libc
   * startup code.
   *
   * So we clear the data_table memory here.
   */
  asm volatile("\t /* assembly code taken from GPLv2+ libgcc.S __do_clear_bss */ \n"
               "\t	ldi     r17, hi8(data_table_end)\n"
               "\t	ldi     r26, lo8(data_table)\n"
               "\t	ldi     r27, hi8(data_table)\n"
               "\t	rjmp    L%=_start\n"
               "\tL%=_loop:\n"
               "\t	st      X+, __zero_reg__\n"
               "\tL%=_start:\n"
               "\t	cpi     r26, lo8(data_table_end)\n"
               "\t	cpc     r27, r17\n"
               "\t	brne    L%=_loop\n"
               ::
               );
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
