# check-segment-sizes.awk - extract information from `avr-size` output
#
# This extracts data about the size of the segments in the ELF file
# from `avr-size` and sums up text+data for giving a number for the
# size of the data to flash.

($1 == "text") {
    # print
    printf("  %7s %5s %5s %5s   %s\n",
	   "txt+dat", $1, $2, $3, $6)
}

($1 ~ /[0-9]+/) {
    # print
    printf("  %7d %5d %5d %5d   %s\n",
	   $1+$2, $1, $2, $3, $6)
}
