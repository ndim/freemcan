#!/usr/bin/Rscript
# plot-hist.R - plot a multi channel histogram data file using R
# Copyright (C) 2010 samplemaker
# Copyright (C) 2010 Hans Ulrich Niedermann
#
#
# Usage:
#
#  ./plot-hist.R data1=<measurement> data2=<background> duration=<[sec]> logplot=<flag>
#
#
# Example usage:
#
# ./plot-hist.R data1=hist.2010-11-01.17:55:01.R.dat data2=hist.2010-11-01.17:34:17.D.dat duration=1200 logplot=1
#
# ./plot-hist.R data1=hist.2010-11-01.17:55:01.R.dat data2= duration=1200 logplot=
#
#


# clear workspace
rm(list = ls())




####################################################################################
# enter global defines here
####################################################################################


#calibration_points <- cbind(c(126,352),c(217,610))
#calibration_points <- cbind(c(87,239),c(212,583))
calibration_points <- cbind(c(73,202),c(111,307))

# some most prominent gamma energies for natural and non natural decay chains (full energy peaks)

chain_name <- "Thorium decay chain"
decay_chain <- cbind(c(239,"Pb-212"),c(338,"Ac-228"),c(510,"Tl-208"),c(583,"Tl-208"),c(727,"Bi-212"),c(911,"Ac-228"),c(965,"Ac-228"),c(1588,"Ac-228"),c(2614,"Tl-208"))


#chain_name <- "Natural uranium chain"
#decay_chain <- cbind(c(184,"U-235"),c(242,"Pb-214"),c(295,"Pb-214"),c(352,"Pb-214"),c(610,"Bi-214"),c(770,"Bi-214"),c(935,"Bi-214"),c(1120,"Bi-214"),c(1240,"Bi-214"),c(1380,"Bi-214"),c(1760,"Bi-214"))

#chain_name <- "Lutetium Lu-176"
#decay_chain <- cbind(c(55,"X-Ray"),c(63,"X-Ray"),c(88,"Lu-176"),c(202,"Lu-176"),c(307,"Lu-176"))
#decay_chain <- cbind(c(202,"Lu-176"),c(307,"Lu-176"))

#chain_name <- "Potassium K-40"
#decay_chain <- cbind(c(1461,"K-40"))

#chain_name <- "Annihilation"
#decay_chain <- cbind(c(512,"Annihilation"))

#chain_name <- "Iod I-131"
#decay_chain <- cbind(c(284,"I-131"),c(364,"I-131"),c(637,"I-131"))

#chain_name <- "Technetium Tc-99m"
#decay_chain <- cbind(c(141,"Tc-99m"))

#chain_name <- "Cobalt Co-60"
#decay_chain <- cbind(c(1173,"Co-60"),c(1333,"Co-60"))

#chain_name <- "Caesium Cs-137"
#decay_chain <- cbind(c(662,"Cs-137"))

#chain_name <- "ALL"
#decay_chain <- cbind(c(141,"Tc-99m"),c(284,"I-131"),c(364,"I-131"),c(512,"Annihilation"),c(637,"I-131"),c(662,"Cs-137"),c(1173,"Co-60"),c(1333,"Co-60"),c(1461,"K-40"))



####################################################################################
# global functions
####################################################################################


# logarithmic ablines
#
loghlines <- function(max){

  numdecades <- ceiling(log10(max))
  for (num in 1:numdecades) {

#    abline(h=seq(0,10^num,10^(num)), lty=3)
#    abline(h=seq(0,10^num,10^(num-1)), lwd=0.2, lty="dashed", col="blue")
    abline(h=seq(0,10^num,10^(num-1)), lwd=0.2, lty="dashed", col="blue")

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
    if ((cmd[[1]][1] == "data1") | (cmd[[1]][1] == "data2")){
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
cat("data1:",data1,"\n")
cat("data2:",data2,"\n")
cat("duration:",duration,"\n")

# read log file as dataframe and move the columns from dataframe into a single
# vector respectively and subtract the last (inclompete) measurement
histdata<-read.table(data1, header=TRUE, sep="\t")
len <-  length(histdata[,1]) - 1
# skip last channel (residual counts)
counts <- histdata[,2][seq(1,len,1)]
index <- histdata[,1][seq(1,len,1)]

if(data2 == FALSE){
  # if there is no background data available set data2 to zero
  len_bg <-  len
  counts_bg <- rep(0, len)
  index_bg <- histdata[,1][seq(1,len,1)]
  # one sigma of the overall distribution and accuracy of the overal mean value:
  overallmean <- 60*sum(histdata[,2])/duration
  overallmeanaccuracy <- sqrt(overallmean)
  residual_counts <- histdata[,2][len+1]
}else{
  histdata_bg<-read.table(data2, header=TRUE, sep="\t")
  len_bg <-  length(histdata_bg[,1]) - 1
  # skip last channel
  counts_bg <- histdata_bg[,2][seq(1,len_bg,1)]
  index_bg <- histdata_bg[,1][seq(1,len_bg,1)]
  # one sigma of the overall distribution and accuracy of the overal mean value:
  overallmean <- 60*sum(histdata[,2]-histdata_bg[,2])/duration
  overallmeanaccuracy <- sqrt(overallmean)
  residual_counts <- histdata[,2][len+1] - histdata_bg[,2][len+1]
}

# configure the plot output and plotting area (2 rows, 1 columns)
 x11(width=13,height=7)
# postscript(width=10, height=6, horizontal=TRUE)
# pdf(width=13, height=7)



par(mfrow=c(1,2))

### in the first chart the gamma spectra minus background is plot
# par(mar=c(5, 5, 5, 5))  # plot area margins

if (logplot){
   plot(index,counts-counts_bg, log = "y", type="l", col="red",
        main = paste("Spectrum without background data"), xlab=" ", ylab=" ",
        ylim=c(1,max(counts)))
   loghlines(max(counts))
}else{
   plot(index,counts-counts_bg, type="h", col="red", main = paste("Spectrum without background data"),
        xlab=" ", ylab=" ", ylim=c(min(0),max(counts)))
}

# two calibration points for energy axis
genergies <- calibration_points
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
mtext("counts per channel", adj=0,side=2, line=2.5,cex=1)
mtext("[keV]", adj=1.07,side=1, line=3.5,cex=1)

legend(x="topright", bty="n",cex=0.8, legend=c(
       paste("Total time elapsed:",duration,"sec"),
       paste("Overall average:",round(overallmean,0),"CPM"),
       paste("Residual counts:", residual_counts),
       "Calibration points @:",
       paste("Chan", genergies[1,1], "/ Energy",genergies[2,1],"keV"),
       paste("Chan", genergies[1,2], "/ Energy",genergies[2,2],"keV")
 ))
 

### second plot: plot unprocessed raw data
plot(index_bg,counts_bg, lwd=0.5,type="l", col="darkblue", main = paste(data2,",",data1),
     xlab=" ", ylab=" ", ylim=c(min(0),max(counts)))
par(new=TRUE)
plot(index, counts , col="darkgreen", type="l", ann=FALSE, yaxt="n",
     ylim=c(min(0),max(counts)))
mtext("counts per channel", adj=0,side=2, line=2.5,cex=1)
axis(1,line=2.5,col="darkgrey",at=tickposition, labels = ticklabel)
mtext("[keV]", adj=1.07,side=1, line=3.5,cex=1)

genergies <- decay_chain
legend(x="topright", bty="n",cex=0.8, legend=c("Prominents from:",chain_name) )

# channelNo <- (genergy-b)/m
channels <- (as.integer(genergies[1,])-b)/m
abline(v=channels, lty="dashed",lwd=1, col="darkgrey")
text(channels, 0.6*max(counts), paste(genergies[2,],"(",genergies[1,],"keV)"), col="black",
     lwd=1, pos=4,cex = 0.6, srt = 90)

# The `locator' function waits for you to either click the mouse
z <- locator(1)
q()

