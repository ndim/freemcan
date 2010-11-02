#!/usr/bin/Rscript
# plot-hist.R - plot a multi channel histogram data file using R
# Copyright (C) 2010 samplemaker
# Copyright (C) 2010 Hans Ulrich Niedermann
#
# Usage: 
#
# Example usage:
#
# ./plot-hist.R filename=measurement.dat bg=background.dat tubesensitivity= duration=1200 logplot=1
#
# # Ludlum  44-2:  tubesensitivity = 0.060584
# # Ludlum  44-11: tubesensitivity = 0.01178
#

# clear workspace

rm(list = ls())

# logarithmic ablines
#
loghlines <- function(max){
  
  numdecades <- ceiling(log10(max))
  for (num in 1:numdecades) {
  
#    abline(h=seq(0,10^num,10^(num-1)), lty=3)
    abline(h=seq(0,10^num,10^(num)), lty=3)
  
  }

}



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
    if ((cmd[[1]][1] == "filename") | (cmd[[1]][1] == "bg")){
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

# brake on incomplete input or set defaults for non specified args
cat("filename:",filename,"\n")
cat("bg:",bg,"\n")
cat("tubesensitivity:",tubesensitivity,"\n")
cat("duration:",duration,"\n")

# read log file as dataframe and move the columns from dataframe into a single 
# vector respectively and subtract the last (inclompete) measurement
histdata<-read.table(filename, header=TRUE, sep="\t")
len <-  length(histdata[,1]) - 1
# skip last channel (residual counts)
counts <- histdata$count[seq(1,len,1)]
index <- histdata$channel[seq(1,len,1)]

if(bg == FALSE){
  # if there is no background data available set bg to zero
  len_bg <-  len
  counts_bg <- rep(0, len)
  index_bg <- histdata$channel[seq(1,len,1)]
  # one sigma of the overall distribution and accuracy of the overal mean value:
  overallmean <- 60*sum(histdata$count)/duration
  overallmeanaccuracy <- sqrt(overallmean)
  residual_counts <- histdata$count[len+1]
}else{
  histdata_bg<-read.table(bg, header=TRUE, sep="\t")
  len_bg <-  length(histdata_bg[,1]) - 1
  # skip last channel
  counts_bg <- histdata_bg$count[seq(1,len_bg,1)]
  index_bg <- histdata_bg$channel[seq(1,len_bg,1)]
  # one sigma of the overall distribution and accuracy of the overal mean value:
  overallmean <- 60*sum(histdata$count-histdata_bg$count)/duration
  overallmeanaccuracy <- sqrt(overallmean)
  residual_counts <- histdata$count[len+1] - histdata_bg$count[len+1]
}

# configure the plot output and plotting area (2 rows, 1 columns)
x11(width=13,height=7)
# postscript(width=13, height=7, horizontal=TRUE)
par(mfrow=c(1,2))

### in the first chart the gamma spectra minus background is plot
# par(mar=c(5, 5, 5, 5))  # plot area margins

if (logplot){
   plot(index,counts-counts_bg, log = "y", type="l", col="red", 
        main = paste("Gamma spectra from logged data"), xlab=" ", ylab=" ", 
        ylim=c(1,max(counts)))
   loghlines(max(counts))
}else{
   plot(index,counts-counts_bg, type="h", col="red", main = paste("Gamma spectra from logged data"), 
        xlab=" ", ylab=" ", ylim=c(min(0),max(counts)))
}

# two calibration points for energy axis
genergies <- cbind(c(88,239),c(213,583))
abline(v=genergies[1,], lty="dashed",lwd=1, col="darkgrey")
text(genergies[1,], counts[genergies[1,]], paste(genergies[2,],"keV"), 
     col="black", lwd=1, pos=4, cex=0.8)

# from the two calibration points calculate the transfer function between channel & energy
# genergy <- b+m*channel
m <- (genergies[2,1]-genergies[2,2])/(genergies[1,1]-genergies[1,2])
b <- genergies[2,2]-genergies[1,2]*m

# calculate a nice looking energy axis labeling
axisenergies = c(b,b+m*max(index))
axiscounts = c(0,length(index))
newaxis <- axisrescaler(axiscounts,axisenergies)
tickposition <- newaxis[,1]
ticklabel <- newaxis[,2]
axis(1,line=2.5,col="darkgrey",at=tickposition, labels = ticklabel)

mtext("total counts", adj=0,side=2, line=2.5,cex=1)

if(tubesensitivity == FALSE){
  legend(x="topright", bty="n",cex=0.8, legend=c(  
         paste("overall average:",round(overallmean,2),"CPM"),
         "overall one sigma-",
         paste("accuracy:+/-",round(overallmeanaccuracy,2),"CPM"),
         paste("total time:",duration,"sec"),
         paste("residual counts:", residual_counts) ))
}else{
  legend(x="topright", bty="n",cex=0.8, legend=c(  
         paste("overall average:",round(overallmean*tubesensitivity,2),"nSv/h"),
         "overall one sigma-",
         paste("accuracy:+/-",round(overallmeanaccuracy*tubesensitivity,2),"nSv/h"),
         paste("total time:",duration,"sec") ))
}


### second plot: plot unprocessed raw data
plot(index_bg,counts_bg, lwd=0.5,type="l", col="darkblue", main = paste("Data:",bg,filename), 
     xlab=" ", ylab=" ", ylim=c(min(0),max(counts)))
par(new=TRUE)
plot(index, counts , col="darkgreen", type="l", ann=FALSE, yaxt="n", 
     ylim=c(min(0),max(counts)))
mtext("total counts", adj=0,side=2, line=2.5,cex=1)
axis(1,line=2.5,col="darkgrey",at=tickposition, labels = ticklabel)

# some most prominent gamma energies for natural decay chains
# Th-232 decay with daughters 
genergies <- cbind(c(239,"Pb-212"),c(338,"Ac-228"),c(510,"Tl-208"),c(583,"Tl-208"),c(727,"Bi-212"),c(911,"Ac-228"),c(965,"Ac-228"),c(1588,"Ac-228"),c(2614,"Tl-208"))

# Ore
#genergies <- cbind(c(184,"U-235"),c(242,"Pb-214"),c(295,"Pb-214"),c(352,"Pb-214"),c(610,"Bi-214"),c(770,"Bi-214"),c(935,"Bi-214"),c(1120,"Bi-214"),c(1240,"Bi-214"),c(1380,"Bi-214"),c(1760,"Bi-214"))

# channelNo <- (genergy-b)/m
channels <- (as.integer(genergies[1,])-b)/m
abline(v=channels, lty="dashed",lwd=1, col="darkgrey")
text(channels, 0.6*max(counts), paste(genergies[2,],"(",genergies[1,],"keV)"), col="black", 
     lwd=1, pos=4,cex = 0.6, srt = 90)

# The `locator' function waits for you to either click the mouse
z <- locator(1)
q()

