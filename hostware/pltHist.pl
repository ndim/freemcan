#!/usr/bin/perl -w
#
#  pltHist.pl
#
#  Plot freeMCAn data - file with newest time stamp (default, call with no arguments)
#  Plot freeMCAn data - from one file given by one argument
#  Plot freeMCAn data - two files given by two arguments
#  Plot freeMCAn data - difference from two files given by argument "-d"
#
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

use warnings;

$datadir = "./";
$tmpfile = "$datadir/tmp.dat";
$numargs = $#ARGV + 1;

SWITCH: {
  #if there is one argument: one file is to be plot
  $numargs == 1 && do {  $plotfile1 = $ARGV[0];
                         print "Print file with arg[0]: $plotfile1 \n";
                         $numplotmode = 1;
                         last SWITCH;
                      };
  #if there are two arguments: two files are to be plot
  $numargs == 2 && do {  $plotfile1 = $ARGV[0];
                         $plotfile2 = $ARGV[1];
                         print "Print file with arg[0] & arg[1]: $plotfile1 $plotfile2 \n";
                         $numplotmode = 2;
                         last SWITCH;
                      };
  #if there are three arguments: calculate difference and plot difference
  $numargs == 3 && do {  if ($ARGV[0] eq "-d"){
                           $plotfile1 = &parse_and_difference_to_tmp($ARGV[1],$ARGV[2]);
                           $numplotmode = 3;
                         }
                         else{
                           die "usage to plot with background subtracted: ./pltHist -d file1 background \n";
                         }
                         last SWITCH;
                      };
  #default fall through: if there are too much or too less arguments we look for the newest file
  opendir DIR, $datadir or die "Plot utility: $datadir cannot be read: $!";
  map { $plotfile1 = $_ } grep { /\.dat/ and -f } sort readdir DIR;
  print "Plotting file with last timestamp: $plotfile1 \n";
  $numplotmode = 1;
}
#start gnuplot
print "Enter to continue \n";
open(GP, "| '/usr/bin/gnuplot' 2>&1 ");
syswrite(GP, "load 'pltOptions.plt' \n");

SWITCH: {
  #single plot (one file)
  $numplotmode == 1 && do {  syswrite(GP, "plot \"$plotfile1\" using 1:2 with lines title \"test\" \n");
                             last SWITCH;
                          };
  #double plot (two files at one time)
  $numplotmode == 2 && do {  syswrite(GP, "plot \"$plotfile1\" using 1:2 with lines , \"$plotfile2\" using 1:2 with lines \n");
                             last SWITCH;
                          };
  #plot with background subtracted
  $numplotmode == 3 && do {  syswrite(GP, "plot \"$plotfile1\" using 1:2 with lines title \"background subtracted\" \n");
                             last SWITCH;
                          };
#this is a poor mans fall through
}
#sleep 5;
<STDIN>;
syswrite(GP, "quit\n");


#subroutine parses two files and writes the data difference to ./tmp.dat
sub parse_and_difference_to_tmp
{
  my $plotfile1 = $_[0];
  my $plotfile2 = $_[1];

  print "Print difference from arg[1] & arg[2]: $plotfile1 - $plotfile2 \n";
  #parse first histogram
  open (FILE1, "<$plotfile1") ||
    die "cannot open $plotfile1 ";
  LINE1: while (<FILE1>) {
    chomp;
    # if the line starts with "#" (the anchor ^ means match
    # at the beginning of the string)
    next LINE1 if ($_ =~ /^#/);
    # numbers have to be separated by a tab
    my @data = split(/\t/,$_);
    $matrix1[$data[0]] = $data[1];
  }
  #  a weaker solution would be:
  #     if (!($_ =~ /\s*#.*/)){
  #        $_ =~ /\s*([0-9]+)\s+([0-9]+)/;
  #        $matrix2[$1]=$2;
  close (FILE1) ||
    die "cannot close $plotfile1 ";
  #parse second histogram
  open (FILE2, "<$plotfile2") ||
    die "cannot open  $plotfile2 ";
  LINE2: while (<FILE2>) {
    chomp;
    # the anchor ^ means match at the beginning of the string with '#'
    next LINE2 if ($_ =~ /^#/);
    # separate numbers between a tab
    my @data = split(/\t/,$_);
    $matrix2[$data[0]] = $data[1];
  }
  close (FILE2) ||
    die "cannot close $plotfile2 ";
  #take both arrays and write difference into ./tmp.dat
  open (TMP, ">$tmpfile") ||
    die "cannot open  $tmpfile ";
  for ($i = 0;$i < @matrix1;$i++){
    $delta = $matrix1[$i] - $matrix2[$i];
    print TMP "$i $delta \n";
  }
  close (TMP) ||
    die "cannot close $tmpfile ";
  return $tmpfile;
}
