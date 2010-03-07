#!/usr/bin/perl -w
#
#  plotHist.pl
#
#  Plot freeMCAn data file with newest time stamp
#  Copyright (C) 2010 samplemaker
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public License
#  as published by the Free Software Foundation; either version 2.1
#  of the License, or (at your option) any later version.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free
#  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
#  Boston, MA 02110-1301 USA

use LWP::Simple;
use File::Basename;

$datadir = "./";
opendir DIR, $datadir or die "Plot utility: $datadir cannot be read: $!";
map { $plotfile = $_ } grep { /hist/ and -f } sort readdir DIR;
print "Plotting the file: $plotfile \n";
print "Enter to continue \n";

open(GP, "| '/usr/bin/gnuplot' 2>&1 ");
syswrite(GP, "load 'pltOptions.plt' \n");
syswrite(GP, "plot \"$plotfile\" using 1:2 with lines title \"test\" \n");

#sleep 5;
<STDIN>;
syswrite(GP, "quit\n");

