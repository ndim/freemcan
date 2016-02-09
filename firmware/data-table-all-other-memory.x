/* Memory layout before this runs:
 *
 *     __noinit_end:    end of uninitialized data
 *
 *     _end:            end of whatever...
 *     __heap_start:    mallocable heap, growing upwards
 *
 *     __heap_end:
 *
 *     RAM_END:         stack, growing downwards
 *
 * Memory layout after this runs:
 *
 *     __noinit_end:    end of unitialized data
 *     data_table:      fixed size data table
 *
 *     data_table_end:
 *     _end:            end of whatever
 *     __heap_start:    mallocable heap, growing upwards
 *
 *     __heap_end:
 *
 *     RAM_END:         stack, growing downwards
 *
 * In both cases, neither malloc nor the stack pointer should go past
 * __heap_end, otherwise you need to verify they never do that at the
 * same time.
 */
SECTIONS {
  data_table = __noinit_end ;
  data_table_end = ( RAM_END - MAX_RUNTIME_STACK_SIZE - MALLOC_HEAP_SIZE ) ;
  data_table_size = data_table_end - data_table ;
  /* Unused definitions for table size in different units
   * data_table_size_by_2 = data_table_size / 2 ;
   * data_table_size_by_3 = data_table_size / 3 ;
   * data_table_size_by_4 = data_table_size / 4 ;
   */
  _end = data_table_end ;
  __heap_start = _end ;
  __heap_end = __heap_start + MALLOC_HEAP_SIZE ;

}
INSERT AFTER .noinit ;
ASSERT ( ( __heap_end + MAX_RUNTIME_STACK_SIZE ) <= RAM_END, "(data+table+heap+stack) size is too large for SRAM (data-table-all-other-memory.x)") ;
ASSERT ( data_table_size >= 1024, "data table size is smaller than 1K (data-table-all-other-memory.x)") ;
