#!/usr/bin/Rscript
# plot-time-series.R - plot a time series dat file using R
# Copyright (C) 2010 samplemaker
# Copyright (C) 2010 Hans Ulrich Niedermann
#
# Usage: 
#
# Example usage:
#
# ./plot-hist.R filename=hist.2010-10-31.19\:21\:42.D.dat tubesensitivity=0.060584 duration=1200
#
# # Ludlum  44-2: tubesensitivity=0.060584
#
#

# clear workspace

rm(list = ls())



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


# read log file as dataframe and move the columns from dataframe into a single 
# vector respectively and subtract the last (inclompete) measurement
histdata<-read.table(filename, header=TRUE, sep="\t")
len <-  length(histdata[,1]) - 1
counts <- histdata$count
index <- histdata$channel

# one sigma of the overall distribution and accuracy of the overal mean value:
overallmean <- 60*sum(counts)/duration
overallmeanaccuracy <- sqrt(overallmean)


# configure the plot output and plotting area (2 rows, 1 columns)
x11(width=10,height=4.5)
#postscript(width=10, height=8, horizontal=TRUE)
par(mfrow=c(1,1))

# in the first plot the filtered rawdata and the unfiltered rawdata is plot including 
# one sigma thresholds 
#par(mar=c(5, 5, 5, 5))  #plot area margins

plot(index,counts, type="l", col="red", main = paste("Datastream from:",filename), 
     xlab=" ", ylab=" ", ylim=c(min(0),max(counts)))

# two calibration points
genergies <- cbind(c(87,202),c(130,307))
abline(v=genergies[1,], lty="dashed",lwd=1, col="blue")
text(genergies[1,], counts[genergies[1,]], genergies[2,], col="black", lwd=1, pos=4)
#text(genergies[1,], max(counts), genergies[2,], col="black", lwd=1, pos=4)

# from the two calibration points calculate the transfer function between channel & energy
m <- (genergies[2,1]-genergies[2,2])/(genergies[1,1]-genergies[1,2])
b <- genergies[2,2]-genergies[1,2]*m

# determine a nice looking labeling of the energy axis
axisenergies = c(b,b+m*max(index))
axiscounts = c(0,length(index))
newaxis <- axisrescaler(axiscounts,axisenergies)
tickposition <- newaxis[,1]
ticklabel <- newaxis[,2]
axis(1,line=2.5,col="darkgrey",at=tickposition, labels = ticklabel)

mtext("total counts", adj=0,side=2, line=2.5,cex=1)

#plot(index, counts ,lwd=0.5, col="darkgreen", type="l", ann=FALSE, yaxt="n", 
#     ylim=c(min(0),max(counts)))


par(new=TRUE)

#countsfiltered <- counts
#plot(index,countsfiltered, type="l", col="red", main = paste("Datastream from:",filename), 
#     xlab=" ", ylab=" ", ylim=c(min(0),max(counts)))


if(tubesensitivity == FALSE){
  legend(x="topright", bty="n",cex=0.8, legend=c(  
         paste("overall average:",round(overallmean,2),"CPM"),
         "overall one sigma-",
         paste("accuracy:+/-",round(overallmeanaccuracy,2),"CPM"),
         paste("total time:",duration,"sec") ))
}else{
  legend(x="topright", bty="n",cex=0.8, legend=c(  
         paste("overall average:",round(overallmean*tubesensitivity,2),"nSv/h"),
         "overall one sigma-",
         paste("accuracy:+/-",round(overallmeanaccuracy*tubesensitivity,2),"nSv/h"),
         paste("total time:",duration,"sec") ))
}


# The `locator' function waits for you to either click the mouse
z <- locator(1)
q()

