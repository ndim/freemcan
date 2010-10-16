SECTIONS {
  __time_series_table_start = __heap_start ;
  __time_series_table_end = ( RAM_END - MIN_STACK_SIZE - MALLOC_HEAP_SIZE ) ;
  __time_series_table_size = __time_series_table_end - __time_series_table_start ;
  __time_series_table_size2 = __time_series_table_size / 2 ;
  __time_series_table_size3 = __time_series_table_size / 3 ;
  __time_series_table_size4 = __time_series_table_size / 4 ;
  __heap_start = __time_series_table_end;
  __heap_end = __heap_start + MALLOC_HEAP_SIZE ;
}
INSERT AFTER .noinit ;
ASSERT ( ( __heap_end + MIN_STACK_SIZE ) <= RAM_END, "(data+stack+table) size is too large for SRAM") ;
