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
# # Ludlum  44-2: tubesensitivity=0.060584 [nSv/h*CPM]
# # VALVO ZP1320: tubesensitivity=76.4     [nSv/h*CPM]
#
#  tubesensitivity  <- doserate [nSv/h] / countrate [CPM] (can be left emtpty)
#  downsamplefactor <- number of samples added together to get a new sample set (can be left emtpty)
#  secondspersample <- count time for each sample
#

# clear workspace

rm(list = ls())



####################################################################################
# enter global defines here
####################################################################################

#order of moving average: mean over 1+2*num_ma samples
num_ma <- 10



####################################################################################
# global functions
####################################################################################



# returns useful tick and label locations based on the given axis to be scaled
#
# limsrc: axis-borders (min max) of the source axis in the plotarea
# limdst: axis-borders (min max) of the destiny axis with the new labels to be plot
#
# tickposition: position of ticks on the source axis
# labels: labels to put on the destiny (new axis labeling)

xdsttoxsrc <- function(xdst,limsrc,limdst){
  return(limsrc[1]+(limsrc[2]-limsrc[1])*(xdst-limdst[1])/(limdst[2]-limdst[1]))
}

axisrescaler <- function(limsrc,limdst) {
  # find out the decade of the destiny axis, choose an amount of
  # ticks depending on the ratio between the new axis height and
  # the decade
  delta <- (limdst[2]-limdst[1])
  numdecades <- floor(log10(delta))
  base <- 10^(numdecades)
  numticks <- floor(10*delta/base)
  if (numticks < 15){increment <- 0.1*base } else {
      if (numticks < 30){increment <- 0.4*base } else {
          if (numticks < 60){ increment <- 0.5*base } else {
              increment <- base }}}
  lowlim <- increment*ceiling(limdst[1]/increment)
  ticklabels <-  seq(lowlim, limdst[2], increment)
  tickposition <- xdsttoxsrc(ticklabels,limsrc,limdst)

  return(cbind(tickposition,ticklabels));
}




# Moving average
#
# x: input data vector
# k: output is the mean from -k to k

ma <- function(x,k) {

  # destiny vector with zeros
  y <- rep(0, length(x))

  # proceed the first view 1 .. k elements of the input vector
  # calculate the 1, 2, 3 ..k moving average
  y[1] <- x[1]
  for (i in 2:k){
     u <- 0
     for (j in 1:(2*i-1)) {
        u <- u + x[j]
     }
     y[i] <- (u/(2.0*i-1.0))
  }

  # proceed mid of the input vector
  for (i in (k + 1):(length(x) - k)) {
     u <- 0
     for (j in (i-k):(i+k)) {
         u <- u + x[j]
     }
     y[i] <- (u/(2.0*k+1.0))
  }

  # proceed the trailing elements of the input vector
  ofs <- length(x)
  for (i in (length(x) - k + 1):(length(x) - 1)) {
     u <- 0
     for (j in (i - (ofs - i)):(i + (ofs - i))) {
         u <- u + x[j]
     }
     y[i] <- (u/(2.0*(ofs-i)+1.0))
  }
  y[length(x)] <- x[length(x)]

  return(y);
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



# Counting Statistics
#
# 1.) The observed count rate is the number of N counts observed
# within one unit observation period.
# 2.) The probability distribution is derived from the uncertainty/spread
# of the observed count rates over unit observation periods. For a
# distribution mean (the expectation or the true count rate)
# N0>50 counts/period the sample counts can be assumed normally distributed
# In case of a counting statistic one will find out the special result:
# 1 sample variance s=sqrt(N0).
# 3.) Knowing the cumulative normal distribution the probability of
# observing a "N count sample" within a band N0 +/- k*s is
# p(k)=erf(k/sqrt(2)).
# 4.) Point 2. can be understood vice versa: For a true count rate N0 to
# be within an error band or confidence interval N +/- k*s where N is the
# observed count rate one can give a confidence level p(k)=erf(k/sqrt(2)).
# Evaluation of the errorfunction gives:
# k=1: 68.27%
# k=2: 95.45%
# k=3: 99.73%
# For e.g. k=1 this means: In 68 measurements (from 100 taken) the true
# doserate N0 is really within the band N observed samples +/- sqrt(N).
# 5.) Normalizing gives a dose rate or normalized sample rate. Increasing
# the amount of taken samples / increasing the measuring time the
# error band for the dose rate at a fix confidence gets smaller. Averaging
# an infinite amount of count rates (infinite sample mean) would give
# the true dose rate.
#
# Examples:
#
# i.) From several sample measurements the total counts measured were
# N=100000 cnts within a total observing time of 600 seconds.
# Give the accuracy.
# -> Normalizing gives N*=10000 cpm. Giving a 95% confidence the true
# count rate N0 is within 10000 cpm +/- 63 cpm. [+/-2*sqrt(N)*(60/600)]
#
# ii.) A doserate shall be measured up to an accuracy of 1% within a
# confidence of 68%.
# -> The doserate (and its error) is proportional to the count rate
# (and its error) within a certain observation period. A confidence of 68%
# gives k=1 and therefore an error of +/- (sqrt(N)/N) which shall be equal to 1%.
# The measurement must at least record N=10000 counts to give the required
# accuracy and confidence.
#
# iii.) From 1000 taken samples we get a sample mean of 400 cnts/period.
# Whats the probability to find one measurement with >500 cnts/period
# within the whole measurement?
# -> For answering this question the expectation N0 (true count rate) is
# unknown. To give an estimate an appropiate estimator is N0=400cnts
# and s=sqrt(400)=20cnts. We get 500cnts - 400cnts = 100 cnts which
# is k=100/20=5 standard deviations. The probability to have an
# observed count rate > 500 cnts in one period unit is
# p=0.5*(1-erf(5/sqrt(2))). The probability to have it within 1000
# measurements is 1000*p or 0.029%.
#

quantile <- function(x) {
  len <- length(x)
  mean <- mean(x)
  sigma  <- sqrt(mean)
  {
  if (mean<50) {
    #if the mean value is below 50 we will get a poisson distribution
    return(mean)
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
len <-  length(histdata[,1])
times <- as.POSIXct(histdata$time_t[1:len], origin="1970-01-01")
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
#pdf(width=10, height=8)
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
# countsfiltered <- pt1(counts, 4)
countsfiltered <- ma(counts, num_ma)

plot(index,countsfiltered, type="l", col="red", main = paste("Datastream from:",filename),
     xlab=" ", ylab=" ", ylim=c(min(0),max(counts)))
mtext(ifelse(tubesensitivity, "[nSv/h]", "[cnts/min]"), side=4,adj=0, line=3)

#prepare the second axis: rescale and calculate the axis ticks
{
if (tubesensitivity == FALSE){
   newaxis <- axisrescaler(c(0,max(counts)), c(0,pfact*max(counts)))
}else{
   newaxis <- axisrescaler(c(0,max(counts)), c(0,pfact*tubesensitivity*max(counts)))
}
}
tickposition <- newaxis[,1]
ticklabel <- newaxis[,2]
axis(4,line=0,col="black",at=tickposition, labels = ticklabel)

# prepare a second x-axis with customized time stamps:
# calculate the elapsed time in minutes and put this time
# to the axisrescaler function to have "nice looking" labels
axisminutes = c((unclass(times[1]))/60,(unclass(times[length(times)])/60))
axiscounts = c(0,length(index))
newaxis <- axisrescaler(axiscounts,axisminutes)
tickposition <- newaxis[,1]
ticklabel <- 60*newaxis[,2]
ticklabel <- as.POSIXct(unclass(ticklabel), origin="1970-01-01")
ticklabel <- format(ticklabel, format = "%H:%M")
axis(1,line=2.5,col="darkgrey",at=tickposition, labels = ticklabel)

if (mean(counts) < 50){
   legend(x="bottomleft", bty="n",cex=0.7, lty=c(1,1), col=c("darkgreen","red"),
          legend=c(paste("Raw data unfiltered [counts per", period, "sec]"),
                   paste(ifelse(tubesensitivity, "Moving average over", "Count rate moving average over"),1+2*num_ma,"samples") ))
}else{
   legend(x="bottomleft", bty="n",cex=0.7, lty=c(1,1,2), col=c("darkgreen","red","blue"),
          legend=c(paste("Raw data unfiltered [counts per", period, "sec]"),
                   paste(ifelse(tubesensitivity, "Moving average over", "Count rate moving average over"),1+2*num_ma,"samples"),
                         "+/-Sqrt(sample mean)"))
}

if(tubesensitivity == FALSE){
 legend(x="bottomright", bty="n",cex=0.7, legend=c(
         paste("Sample mean (normalized):",round(overallmean,2),"cpm"),
         paste("95.4% confidence level: +/-",signif(2.0*overallmeanaccuracy,2),"cpm")))


}else{
  legend(x="bottomright", bty="n",cex=0.7, legend=c(
         paste("Sample mean (normalized):",round(overallmean*tubesensitivity,2),"nSv/h"),
         paste("95.4% confidence level: +/-",signif(2.0*overallmeanaccuracy*tubesensitivity,2),"nSv/h")))
}

# a histogram is created from data and plotted in the second plot area.

if (mean(counts) < 50){
  #poisson distribution coloring
  # p(x) = lambda^x exp(-lambda)/x!
  # E(X) = Var(X) = lambda

   hx <- hist(counts,breaks=seq(min(counts)-0.5, max(counts)+0.5, 1), plot = FALSE)
   cntsd <- sd(counts)
   cntmean <- q[1]
   xhist <- hx$mids
   yhist <- hx$density
   #get the propability density function of poisson distribution. only integers allowed for dpois
   xfit <- seq(min(counts),max(counts))
   yfit <- dpois(xfit,lambda=cntmean)

   plot(xhist,yhist,type="h",lwd=1,ylim=c(0,max(yhist,yfit)),
        col="blue",
        xlab=paste("Expected poisson pdf and histogram of raw data [samples per", period,
        "seconds]"),ylab="Density",main="Goodness of fit")
   #draw theoretical density function according to mean value only (color: green)
   lines(xfit,yfit, col="darkgreen",lwd=1,type="b")
   #lines(density(counts), col="red")
   legend(x="topleft", bty="n",cex=0.7, lty=c(1,1), col=c("darkgreen","blue"),
       legend=c("Theoretical fit based on sample mean",
                "Measured distribution"))

   legend(x="topright", bty="n",cex=0.7,
         legend=c(paste("Sample mean:",round(cntmean,2),"cnts/per")
		  ))

}else{
   #gaussian distribution
   hx <- hist(counts,breaks=seq(min(counts)-0.5, max(counts)+0.5, 1), plot = FALSE)
   cntsd <- sd(counts)
   cntmean <- q[3]
   xhist <- hx$mids
   yhist <- hx$density
   xfit <- seq(min(counts),max(counts),length=100)
   yfit <- dnorm(xfit,mean=cntmean,sd=sqrt(mean(counts)))
   #draw measured histogram including measured standart deviation
   #type h,s
   plot(xhist,yhist,type="h",lwd=1,ylim=c(0,max(yhist,yfit)),
#        col=ifelse(((hx$breaks < (cntmean-cntsd)) | (hx$breaks > (cntmean+cntsd))),"blue", "red"),
        col="blue",
         xlab=paste("Expected normal pdf and histogram of raw data [samples per", period,
	 "seconds]"),ylab="Density",main="Goodness of fit")
   #lines(density(counts), col="red")

   #draw theoretical density function according to mean value only (color: green)
   lines(xfit,yfit, col="darkgreen",lwd=1)
   lines(c(q[1],q[1]),c(0,dnorm(q[1],mean=q[3],sd=sqrt(mean(counts)))),
         col="darkgreen", lty="dashed", lwd=2)
   lines(c(q[2],q[2]),c(0,dnorm(q[2],mean=q[3],sd=sqrt(mean(counts)))),
         col="darkgreen", lty="dashed", lwd=2)
   lines(c(q[3],q[3]),c(0,dnorm(q[3],mean=q[3],sd=sqrt(mean(counts)))),
         col="darkgreen", lty="dashed", lwd=2)
   #text(q, max(yhist,yfit), round(q,1), col="black", lwd=1, pos=4)

  legend(x="topleft", bty="n",cex=0.7, lty=c(1,1), col=c("darkgreen","blue"),
       legend=c("Theoretical fit based on sample mean",
                "Measured distribution"))

  legend(x="topright", bty="n",cex=0.7,
         legend=c(paste("Sample mean:",round(cntmean,1),"cnts/per"),
	          paste("Sample variance:",round(cntsd,1),"cnts/per"),
	          paste("Sqrt(sample mean):",round(sqrt(mean(counts)),1),"cnts/per")
		  ))

}



# The `locator' function waits for you to either click the mouse
z <- locator(1)
q()

