#  plot_freemcan.plt
#
#  Gnuplot commando file. Plots freemcan-export.dat.
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

####################################################################################################
# run this file as follows:
# gnuplot> load 'plot_freemcan.plt'
####################################################################################################

reset;

####################################################################################################
# Prepare plot area
####################################################################################################

set title "FreeMCAn-Spectrograms"
set xlabel 'Channel No.'
set ylabel 'Counts'
set xrange [0:511]
set logscale y
set grid ytics lt 0 lw 1 lc rgb "#709070"
set grid xtics lt 0 lw 1 lc rgb "#709070"
#set nologscale y
set size ratio 0.45

####################################################################################################
# Data output options e.g. if you want to export to postscript
####################################################################################################

#set terminal postscript eps enhanced color solid;
#set output "freemcan-export.ps"
#set terminal x11

####################################################################################################
# Plot the data
####################################################################################################

#plot "freemcan-export.dat" with lines
plot "freemcan-export.dat" with dots
