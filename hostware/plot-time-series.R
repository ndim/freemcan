#!/usr/bin/Rscript
# plot-time-series.R - plot a time series dat file using R
# Copyright (C) 2010 samplemaker
# Copyright (C) 2010 Hans Ulrich Niedermann
#
# Usage: ./plot-time-series.R tbd
#
# Example usage:
#
# ./plot-time-series.R secondspersample=150 filename=blubber.dat downsamplefactor= tubesensitivity=
#
#  tubesensitivity  <- doserate [nSv/h] / countrate [CPM] (can be left emtpty)
#  downsamplefactor <- number of samples added together to get a new sample set (can be left emtpty)
#  secondspersample <- count time for each sample
#

# clear workspace

rm(list = ls())



# returns useful tick and label locations based on the given axis to be scaled
#
# lim: maximum point of the axis to be drawn (on the source plot in the source data set)
#
# tickposition: position of ticks on the source axis
# labels: labels to put on the destiny (new axis labeling)
# \fixme: implement lim=c(min,max) to support not only axis beginning from "0"

axisrescaler <- function(lim,scalefactor) {
  # find out the decade of the destiny axis and choose an increment
  # so that we get a "good looking scale"
  maxdstlim <- lim*scalefactor
  delta <- abs(maxdstlim)
  numdecades <- floor(log10(delta))
  base <- 10^(numdecades)
  numticks <- floor(maxdstlim/base)
  if (numticks<=5){
    increment <- base
  }
  else{
      increment <- 0.5*base
  }
  ticklabels <-  seq(0, maxdstlim, increment)
  tickposition <- ticklabels/scalefactor

  return(cbind(tickposition,ticklabels));
}



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



# calculate counting statistics of the input vector
# 
# the true value of the count rate (or doserate) is within a probability of
# 68,3 % within a range of the mean value +/- its one sigma := square root(mean).
# the function therefore outputs the overal average and both mean+/-one sigma 
# quantiles
#
# if the mean value is less than 50 a poisson distribution occurs rather than
# a gaussian distribution. in this case the function returns only one sigma threshold

quantile <- function(x) {
  len <- length(x)
  mean <- mean(x)
  sigma  <- sqrt(mean)
  {
  if (mean<50) {
    #if the mean value is below 50 we will have rahter a poisson distribution
    return(c(mean-sigma, mean))
  }
  else{
    #if not we are going to get a gaussian distribution
    return(c(mean-sigma, mean+sigma, mean))
  }
  }
}


####################################################################################
# start of program
####################################################################################

# parse command line arguments

args <- commandArgs(TRUE)
for (numarg in 1:length(args)) {
  cmd = strsplit(args[numarg],split="=")
  if(! is.na(cmd[[1]][2])) {
    if (cmd[[1]][1] == "filename"){
      assign(cmd[[1]][1],cmd[[1]][2])
    }
    else{
      assign(cmd[[1]][1],as.numeric(cmd[[1]][2]))
    }
  }
  else{
    assign(cmd[[1]][1],FALSE)
  }
}

# brake on incomplete input or set defaults for not specified args
cat("seconds per sampel:", secondspersample, "\n")
cat("filename:", filename, "\n")
if (downsamplefactor == "FALSE"){downsamplefactor <- 1}
cat("downsample factor:", downsamplefactor, "\n")
cat("tube sensitivity:", tubesensitivity, "\n")

period <- downsamplefactor * secondspersample
pfact <- 60.0 / period

# read log file as dataframe and move the columns from dataframe into a single 
# vector respectively and subtract the last (inclompete) measurement
histdata<-read.table(filename, header=TRUE, sep="\t")
len <-  length(histdata[,1]) - 1
times <- as.POSIXlt(histdata$time_t[1:len], origin="1970-01-01")
counts <- histdata$counts[1:len]
index <- histdata$idx[1:len]

# one sigma of the overall distribution and accuracy of the overal mean value:
overallmeanaccuracy <- (pfact*downsamplefactor)*sqrt(sum(counts))/len
overallmean <- (pfact*downsamplefactor)*sum(counts)/len

# if downsampling is performed replace data vector and the measurement number 
# with the resized data
counts <- downsample(counts, downsamplefactor)
index <- index[seq(1, length(index)-downsamplefactor, downsamplefactor)]/downsamplefactor
times <- times[seq(1, length(times)-downsamplefactor, downsamplefactor)]

q <- quantile(counts)

# configure the plot output and plotting area (2 rows, 1 columns)
x11(width=10,height=8)
#postscript(width=10, height=8, horizontal=TRUE)
par(mfrow=c(2,1))

# in the first plot the filtered rawdata and the unfiltered rawdata is plot including 
# one sigma thresholds 
par(mar=c(5, 5, 5, 5))  #plot area margins
plot(index, counts ,lwd=0.5, col="darkgreen", type="l", ann=FALSE, yaxt="n", 
     ylim=c(min(0),max(counts)))
abline(h=q,lwd=0.5, lty="dashed", col="blue")
mtext(paste("[counts /", period, "sec]"), adj=0,side=2, line=3,cex=1)
par(new=TRUE)

# to have both curves on the same place in the plot area we do not rescale the curve itself
# but only the axis. so we first of all make a second plot but without plotting the axis
countsfiltered <- pt1(counts,4)
plot(index,countsfiltered, type="l", col="red", main = paste("Datastream from:",filename), 
     xlab=" ", ylab=" ", ylim=c(min(0),max(counts)))
mtext(ifelse(tubesensitivity, "[nSv/h]", "[cnts/min]"), side=4,adj=0, line=3)

#prepare the second axis: rescale and calculate the axis ticks
newaxis <- axisrescaler(max(counts), ifelse(tubesensitivity, pfact*tubesensitivity, pfact))
tickposition <- newaxis[,1]
ticklabel <- newaxis[,2]
axis(4,line=0,col="black",at=tickposition, labels = ticklabel)

#prepare a second x-axis with customized time stamps:
tickposition <- seq(1, length(index)-1, (length(index)-1)/6)
ticklabel = format(times[tickposition], format = "%H:%M")
axis(1,line=2.5,col="grey",at=tickposition, labels = ticklabel)

legend(x="bottomleft", bty="n", lty=c(1,1), col=c("darkgreen","red"), 
       legend=c(paste("raw data unfiltered [counts per", period, "sec]"), 
       ifelse(tubesensitivity, "doserate filtered [nSv/h]", "count rate filtered [CPM]")))

# a histogram is created from data and plotted in the second plot area.
# the histogram is plot blue if the bar is below one sigma otherwise red
hx <- hist(counts, breaks = 90, plot = FALSE)
{
if (overallmean < 50){
  #poisson distribution coloring
  plot(hx,col=ifelse(((hx$breaks < q[1])), "blue", "red") ,
       xlab=paste("rawdata of", period, "seconds)"))
}
else{
  #gaussian distribution coloring
  plot(hx,col=ifelse(((hx$breaks < q[1]) | (hx$breaks > q[2])), "blue", "red"),
       xlab=paste("rawdata of", period, "seconds)"))
}
}
abline(v=q, col="darkgreen", lwd=1)
text(q, max(hx$counts), round(q,1), col="black", lwd=1, pos=4)

{
if(tubesensitivity == FALSE){
  legend(x="topright", bty="y",cex=0.8, legend=c(  
         paste("overall average:",round(overallmean,0),"CPM"),
         "overall one sigma-",
         paste("accuracy:+/-",round(overallmeanaccuracy,2),"CPM")))
}
else{
  legend(x="topright", bty="y",cex=0.8, legend=c(  
         paste("overall average:",round(overallmean*tubesensitivity,0),"nSv/h"),
         "overall one sigma-",
         paste("accuracy:+/-",round(overallmeanaccuracy*tubesensitivity,2),"nSv/h")))
}
}

# The `locator' function waits for you to either click the mouse
z <- locator(1)
q()

