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



# Infinite impulse response filter of first order (PT1)
#
# x: input data vector
# k: ratio of halflife/sampling rate - ratio (k = T/Tsample)

pt1 <- function(x,k) {
  Ts <- 1
  T <- k*Ts
  n <- 2*T/Ts;
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
    res = c(res, sum(x[i:(i+k-1)]))
  }
  return(res)
}



# calculate statistics of the input vector
# 
# assuming the input vector has a gaussian distribution (> 50 for each count)
# one sigma is equal to the square root of the average
#

quantile <- function(x) {
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


# read log file as dataframe and move the columns from dataframe into a single 
# vector respectively and subtract the last (inclompete) measurement

histdata<-read.table(filename, header=FALSE, sep="\t")
len <-  length(histdata[,1]) - 1
channel <- histdata[,1][0:len]
counts <- histdata[,2][0:len]

# one sigma of the overall distribution and accuracy of the overal mean value:
overallmeanaccuracy <- (pfact*downsamplefactor)*sqrt(sum(counts))/len
overallmean <- (pfact*downsamplefactor)*sum(counts)/len

# if downsampling is required replace data vector and the measurement number with the resized data

counts <- downsample(counts, downsamplefactor)
channel <- channel[seq(1, length(channel)-downsamplefactor, downsamplefactor)]
pfcounts = pfact * counts
q <- quantile(counts)
cat("Quantile:", "min", q[1], "max", q[2], "mean", q[3], "\n")
pfq <- pfact * q
cat("Quantile:", "min", pfq[1], "max", pfq[2], "mean", pfq[3], "\n")


# configure the plot output and plotting area (2 rows, 1 columns)

x11(width=10,height=8)
#postscript(width=8.5, height=11, horizontal=FALSE)
par(mfrow=c(2,1))


# in the first plot the filtered rawdata and the unfiltered rawdata is plot including one sigma thresholds 
par(mar=c(5, 5, 5, 5)) 
plot(channel,pt1(pfcounts,4), type="l", col="red", main = paste("Datastream from:",filename), 
     xlab="plot of data", ylab=" ", ylim=c(min(0),max(pfcounts)))
mtext(paste("[counts /", period, "sec]"), side=2, line=3,cex=1)
par(new=TRUE)
plot(channel, counts ,lwd=0.5, col="darkgreen", type="l", ann=FALSE, yaxt="n", 
     ylim=c(min(0),max(counts)))
abline(h=q,lwd=0.5, lty="dashed", col="blue")
#axis(2,line=2,col="grey")
axis(4)
legend(x="bottomleft", bty="n", lty=c(1,1), col=c("darkgreen","red"), 
       legend=c(paste("raw data unfiltered [counts per", period, "sec]"), "count rate [CPM] / doserate [nSv/h] filtered"))
mtext("[cnts/min] or [nSv/h]", side=4, line=3)


# a histogram is created from data and plotted in the second plot area.
# the histogram is plot blue if the bar is below one sigma otherwise red

# \fixme: if the mean value is < 50 we do have a poisson distribution and therfore
# the right (+ sigma) threshold and color must be erase (if else)

hx <- hist(counts, breaks = 90, plot = FALSE)
plot(hx, col = ifelse(((hx$breaks < q[1]) | (hx$breaks > q[2])), "blue", "red"), 
     xlab=paste("rawdata of", period, "seconds)"))
abline(v=q, col="darkgreen", lwd=1)
text(q, max(hx$counts), round(q,1), col="black", lwd=1, pos=4)
legend(x="topleft", bty="y",cex=0.8, legend=c(  
       paste("overall mean:",round(overallmean,0),"CPM"),
       "overall one sigma",
       paste("accuracy:+/-",round(overallmeanaccuracy,2),"CPM") ))


# The `locator' function waits for you to either click the mouse
z <- locator(1)
q()

