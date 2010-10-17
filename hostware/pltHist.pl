#!/usr/bin/perl -w
#
#  plotHist.pl
#
#  Plot freeMCAn data - file with newest time stamp (default, no argument)
#  Plot freeMCAn data - from one file given by argument
#  Plot freeMCAn data - two files given by argument
#  Plot freeMCAn data - difference from two files given by argument -d
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

use LWP::Simple;
use File::Basename;

$datadir = "./";
$numargs = $#ARGV + 1;

SWITCH: {
  #if there is one argument: one file is to be plot
  $numargs == 1 && do {  $plotfile1 = $ARGV[0];
                         print "Print file with arg[0]: $plotfile1 \n";
			 $numfiles = 1;
                         last SWITCH;
                      };
  #if there are two arguments: two files are to be plot
  $numargs == 2 && do {  $plotfile1 = $ARGV[0];
                         $plotfile2 = $ARGV[1];
                         print "Print file with arg[0] & arg[1]: $plotfile1 $plotfile2 \n";
			 $numfiles = 2;
                         last SWITCH;
                      };
  #if there are three arguments: calculate difference and plot difference
  $numargs == 3 && do {  if ($ARGV[0] eq "-d"){
                           $plotfile1 = $ARGV[1];
			   $plotfile2 = $ARGV[2];
			   print "Print difference from arg[1] & arg[2]: $plotfile1 $plotfile2 \n";
                           #parse first histogram
			   open (FILE1, "<$plotfile1") ||
			       die "cannot open $plotfile1 ";
                            while (<FILE1>){
			         if (!($_ =~ /\s*#.*/)){
                                    $_ =~ /\s*([0-9]+)\s+([0-9]+)/;
				    $matrix1[$1]=$2;
				 }
			    }
			    close(FILE1) ||
			       die "cannot close $plotfile1 ";
                           #parse second histogram
			   open (FILE2, "<$plotfile2") ||
			       die "cannot open  $plotfile2 ";
                            while (<FILE2>){
			         if (!($_ =~ /\s*#.*/)){
                                    $_ =~ /\s*([0-9]+)\s+([0-9]+)/;
				    $matrix2[$1]=$2;
				 }
			    }
                            #take both array and write difference into TMP
			    $tmpfile = "$datadir/tmp.dat";
			    open (TMP, ">$tmpfile") ||
			     	die "cannot open  $tmpfile ";                            
                            for ($i=0;$i<@matrix1;$i++){
			        $delta = $matrix1[$i] - $matrix2[$i];
                                #print "$delta\n";
                                print TMP "$i $delta \n";
                            }
                            close (TMP) ||
                                die "cannot close $tmpfile ";

                            $plotfile1 = $tmpfile;
                            $numfiles = 1;
			 }
			 else{
			   die "panic! usage: ./pltHist -d file1 file2 \n";
			 }

                         last SWITCH;
                      };

		      
  #default fall through: if there are too much or too less arguments we look for the newest file
  opendir DIR, $datadir or die "Plot utility: $datadir cannot be read: $!";
  map { $plotfile1 = $_ } grep { /hist/ and -f } sort readdir DIR;
  print "Plotting file with last timestamp: $plotfile1 \n";
  $numfiles = 1;
}

#start gnuplot
print "Enter to continue \n";
open(GP, "| '/usr/bin/gnuplot' 2>&1 ");
syswrite(GP, "load 'pltOptions.plt' \n");

#multi plot (two files)
if ($numfiles == 2) {
    syswrite(GP, "plot \"$plotfile1\" using 1:2 with lines , \"$plotfile2\" using 1:2 with lines \n");
}
#single plot (one file)
else {
    syswrite(GP, "plot \"$plotfile1\" using 1:2 with lines title \"test\" \n");
}

#sleep 5;
<STDIN>;
syswrite(GP, "quit\n");

