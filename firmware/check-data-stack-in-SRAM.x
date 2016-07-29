/* We have the dummy SECTIONS and INSERT AFTER in order to make
 * ld execute this late enough (i.e. after __bss_end has actually
 * been set).
 */
SECTIONS {
}
INSERT AFTER .noinit ;
/* We do not check for mallocable heap space here. Why? */
ASSERT( (_end + MAX_RUNTIME_STACK_SIZE) <= RAM_END, "(data+stack) is too large for SRAM (check-data-stack-in-SRAM.x)");
ASSERT( (__heap_start + MALLOC_HEAP_SIZE + MAX_RUNTIME_STACK_SIZE) <= RAM_END, "(data+heap+stack) size is too large for SRAM (check-data-stack-in-SRAM.x)");
