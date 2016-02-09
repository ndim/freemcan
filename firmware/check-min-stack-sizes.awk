# check-min-stack-sizes.awk - extract information from `objdump -h -S` output
#
# This extracts data about the stack usage from an ELF firmware image
# parsed via `avr-objdump -h -S` and checks that the code does not
# exceed a certain value.

BEGIN {
    error_count = 0
    print("CHECK: PUSHES+2*CALLS <= MAX_STACK_SIZE  filename")
}

function beginfile(filename) {
    call_count  = 0
    rcall_count = 0
    push_count  = 0
}

/^[[:blank:]]*[0-9a-f]+:[[:blank:]][0-9a-f[:blank:]]+[[:blank:]]call[[:blank:]]/ {
    call_count++
}

/^[[:blank:]]*[0-9a-f]+:[[:blank:]][0-9a-f[:blank:]]+[[:blank:]]rcall[[:blank:]]/ {
    rcall_count++
}

/^[[:blank:]]*[0-9a-f]+:[[:blank:]][0-9a-f[:blank:]]+[[:blank:]]push[[:blank:]]/ {
    push_count++
}

function endfile(filename) {
    all_call_count = call_count + rcall_count
    if ((all_call_count == 0) || (push_count == 0)) {
	error_count++;
	s = "ERR!"
	c = "XX"
	lhs_count = 0
    } else {
	lhs_count = push_count+2*all_call_count
	if (lhs_count <= MAX_RUNTIME_STACK_SIZE) {
	    s = "PASS"
	    c = "<="
	} else {
	    s = "FAIL"
	    c = " >"
	    error_count++
	}
    }
    printf("  %s %3d+2*%2d = %3d %s %3d  %s\n",
	   s, push_count, all_call_count, lhs_count,
	   c, MAX_RUNTIME_STACK_SIZE, filename)
}

END {
    if (error_count > 0) {
	printf("Error: %d checks failed.\n", error_count);
	exit 2
    }
}
