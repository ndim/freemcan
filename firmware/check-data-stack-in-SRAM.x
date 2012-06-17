ASSERT(__bss_end + MAX_RUNTIME_STACK_SIZE < RAM_END, "(data size + stack size) is too large for SRAM");
