#!/usr/bin/python

import re
import sys
import time

def main():
    ifile = open(sys.argv[1], "r")
    o = sys.stdout
    d = {}
    firstline = True
    for line in ifile.readlines():
        if len(line) == 0:
            pass
        elif line[0] == "#":
            m = re.match("^# ([a-zA-Z0-9_ ]+):\s+(\d+)\s+(.*)", line)
            if m:
                key = m.group(1)
                value = m.group(2)
                d[key] = value
            o.write(line)
        elif line[0].isdigit():
            if firstline:
                dpv = int(d['duration per value'])
                ld = int(d['duration of last value'])
                m = int(d['number of intervals'])
                rt = int(d['receive_time'])
                rtd = rt - ld
                j = m-1
                o.write("%s\n" % "\t".join(["index", "counts", "time_t", "rfc3339"]))
                firstline = False
            v = line.rstrip().split('\t')
            ts = rtd-j*dpv
            v.append(str(ts))
            v.append(time.strftime("%Y-%m-%d %H:%M:%S%z", time.gmtime(ts)))
            j = j - 1
            o.write("%s\n" % "\t".join(v))
        else:
            v = line.split('\t')
            o.write("H %s" % line)

if __name__ == "__main__":
    main()
