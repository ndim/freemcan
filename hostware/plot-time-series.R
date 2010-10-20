#!/usr/bin/Rscript
# plot-time-series.R - plot a time series dat file using R
# Copyright (C) 2010 samplemaker
# Copyright (C) 2010 Hans Ulrich Niedermann
#
# Usage: ./plot-time-series.R <period> <filename>
#
# Example usage:
#
#  $ ./plot-time-series.R 150 time.foobar.dat
#
#    Plot a data file of "time.foobar.dat" recorded with 150 seconds
#    per value.
#
#  $ ./plot-time-series.R 30 $(ls -1 time.*.dat | tail -n1)
#
#    Plot the latest file, presuming it is recorded at 30sec/value.
#
#  $ ./plot-time-series.R 30 4 time.foo.dat
#
#    Plot the file, downsampling by a factor of 4, i.e. from the
#    30sec/value to 120sec/value.
#


# clear workspace
rm(list = ls())


# this is a poor mans infinite impulse response filter of first order (PT1).
pt1  <- function(x,k) {
  Ta <- 1
  T <- k*Ta
  n <- 2*T/Ta;
  a <- 1/(n+1);
  b <- (n-1)/(n+1);

  z <- x[1]/(2.0*a);
  for (i in 1:length(x)) {
    m <- x[i];
    tmp <- b*z;
    x[i] <- a*(m+tmp+z);
    z <- tmp+m;
  }
  return(x);
}

# Simply downsample by an integer factor of k by summing up
# neighbouring values like
#
#      1 ...   k -> 1
#    k+1 ... 2*k -> 2
#  2*k+1 ... 3*k -> 3
#  etc.
downsample <- function(x, k) {
  res <- c()
  for (i in seq(1, length(x)-k, k)) {
    res = c(res, sum(x[seq(i, i+k-1)]))
  }
  return(res)
}


# calculate theoretical one sigma quantil of the distribution
quantil <- function(x) {
  len <- length(x)
  cat("samples parsed:", len, "\n")
  mean <- mean(x)
  cat("mean value:", mean, "\n")
  sigma  <- sqrt(mean)
  cat("your one sigma probability is within: +/- ", sigma, " counts \n")
  return(c(mean-sigma, mean+sigma, mean));
}


# \fixme: one should filter the correct command rather than give a fix position
args <- commandArgs(TRUE)
filename <- ifelse(length(args) == 2, args[2], args[3])
downsamplefactor <- ifelse(length(args) == 2, 1, as.numeric(args[2]))
period <- downsamplefactor * as.numeric(args[1])
cat("Period:", period, "\n")
cat("Filename:", filename, "\n")
cat("Downsample factor:", downsamplefactor, "\n")

pfact <- 60.0 / period

# read log file as dataframe
histdata<-read.table(filename, header=FALSE, sep="\t")
# move the columns from dataframe into a single vector respectively and subtract the last (inclompete) measurement
len <-  length(histdata[,1]) - 1
channel <- histdata[,1][0:len]

counts <- downsample(histdata[,2][0:len], downsamplefactor)
# FIXME: Select the proper channels
length(channel) <- length(counts)
pfcounts = pfact * counts
q <- quantil(counts)
cat("Quantil:", "min", q[1], "max", q[2], "mean", q[3], "\n")
pfq <- pfact * q
cat("Quantil:", "min", pfq[1], "max", pfq[2], "mean", pfq[3], "\n")

x11(width=10,height=8)
# 2 rows, 1 columns.
par(mfrow=c(2,1))

# plot low pass filtered counts over measurement
plot(channel,pt1(pfcounts,5) , main = paste("Raw data from:",filename), type = "l",col = "red",xlab = "Number of measurement",ylab=paste("cpm (scaled from", period, "seconds)"), ylim=c(min(pfcounts),max(pfcounts)))
# plot raw counts over measurement into the same first plot
lines(channel, pfcounts , type="l", col="darkgreen")
# plot mean +- sigma lines
abline(h=pfq, col="blue")

# from the data create a histogram
hx <- hist(pfcounts, breaks = 90, plot = FALSE)
# plot the histogram into the second plot. plot blue if the bar is below one sigma otherwise red
plot(hx, col = ifelse(((hx$breaks < pfq[1]) | (hx$breaks > pfq[2])), "blue", "red"), xlab=paste("cpm (counts per minute, scaled from original measurement period of", period, "seconds)"))
# plot mean +- sigma lines
abline(v=pfq, col="darkgreen", lwd=2)
text(pfq, max(hx$counts), round(pfq,1), col="black", lwd=1, pos=4)

# The `locator' function waits for you to either click the mouse or hit enter (putting coordinates into x & y)
z <- locator(1)
q()

