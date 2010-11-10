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


/* import the symbols in data type inspecific way */
extern volatile char data_table[];
extern volatile char data_table_end[];
extern volatile char data_table[];


/** Setup, needs to be called once on startup */
void data_table_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void data_table_init(void)
{
  /** As the table is outside of the memory area with the normal data,
   * its content will NOT be cleared by the default avr-libc startup
   * code.  So we clear the table memory ourselves.
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
