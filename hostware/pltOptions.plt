#  pltOptions.plt
#
#  Gnuplot options
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

set title "Histogram" font "Arial,16"
#set logscale y
#set nologscale y
#set yrange [0:700]
set ylabel 'Counts'
set grid ytics lt 0 lw 1 lc rgb "#909070"
set xrange [0:1022]
set xlabel 'Channel No.'
set grid xtics lt 0 lw 1 lc rgb "#909070"
set x2tics ("xy keV" 93,"xz keV" 139)
set x2tics rotate 
set grid x2tics lt 0 lw 1 lc rgb "#909070"
set x2label "Energy [keV]"
set size ratio 0.45
#set multiplot
set nomultiplot
set key top right
set key box
#set terminal postscript eps enhanced color solid;
#set output "export.eps"
#plot "hist1.dat" lt 3 with lines title "hits1" , \
#"hist2.dat" lt 2 with lines title "hist2" , \
#"hist3.dat" using 2 with histograms title "xyz"
