ASSERT(__bss_end + MIN_STACK_SIZE < RAM_END, "(data size + stack size) is too large for SRAM");
