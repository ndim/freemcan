function assert_unset(value, string)
{
    if (!(value == -1)) {
	printf("%s:%d: assertion failed: %s == -1\n",
	       FILENAME, FNR, string) > "/dev/stderr"
	_assert_exit = 1
	exit 1
    }
}

END {
    if (_assert_exit) {
	exit 1
    }
}

BEGIN {
    print("________/data_table\\________  filename")
}

function beginfile(filename) {
    start = -1
    table_size = -1
    element_size = -1
}

($6 == "data_table") {
    assert_unset(table_size, "table_size")
    table_size = strtonum("0x"$5)
}

($5 == "data_table") {
    assert_unset(start, "start")
    start = strtonum("0x"$1)
}

($5 == "data_table_end") {
    assert_unset(table_size, "table_size")
    table_size = strtonum("0x"$1) - start
}

($6 == "table_element_bits") {
    assert_unset(element_size, "element_size")
    element_size = strtonum("0x"$5)
}

function endfile(filename) {
    element_count = table_size * 8 / element_size
    printf("%5d bytes = %5d * %2d bit  %s\n",
	   table_size, element_count, element_size, filename)
}
