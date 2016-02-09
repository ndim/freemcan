/* We have the dummy SECTIONS and INSERT AFTER in order to make
 * ld execute this late enough (i.e. after __bss_end has actually
 * been set).
 */
SECTIONS {
}
INSERT AFTER .noinit ;
ASSERT(__bss_end + MAX_RUNTIME_STACK_SIZE < RAM_END, "(data+stack) is too large for SRAM (check-data-stack-in-SRAM.x)");
