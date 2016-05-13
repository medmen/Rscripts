#!/usr/bin/Rscript --slave
#####
# What is it?
#
# This Rscript calculates 95% confidence intervalls for a given column of values, 
# using the bootstrap estimate method, nicely explained here:
# http://www.stat.ucla.edu/~rgould/110as02/bsci
# 
# @input full path to a csv-file holding 1 or more columns of values
# @input header/name of the "target" column, ThIs is CaSe SensiTiVe
# 
# @output upper and lower confidence interval
#####

### tell how to use
usage <- "usage: Rscript confidence.R <file-with-input-data.csv> <column-name>"

### handle command line arguments (i.e. name of data file)
args <- commandArgs(TRUE)
inputfile <- args[1]
targetcolumn <- args[2]

#### open file
# read.csv expects fields separated by komma (",") and the dot (".") as decimal separator
# read.csv2 expects fields separated by semicolon (";") and the komma (",") as decimal separator
#
# Options used:
# header=TRUE: treat first line als header, name colums accoringly
# na="NA": fill empty cells with "NA" (not available)
# check.names=FALSE: by default header names will be sanitized, here we prevent this
incomingData <- read.csv2(inputfile, header=TRUE, na="NA", check.names=FALSE) 

# give names of headers
headers <- names(incomingData)

# attach datafile so we can refer to variable names directly
# don't forget to use detach at the end
attach(incomingData)

tgt <- incomingData[, targetcolumn]
tgt <- tgt[!is.na(tgt)] # sometimes a variable holds NAs, remove those
cnt <- length(tgt)

#### the long version ###
#mean(tgt)
#bstrap <- c()
#for (i in 1:1000){
#  # First take the sample
#  bsample <- sample(tgt,cnt,replace=T)
#  #now calculate the bootstrap estimate
#  bestimate <- mean(bsample)
#  bstrap <- c(bstrap,bestimate)
#}
#ql <- quantile(bstrap,.025)
#qh <- quantile(bstrap,.975)

### short version
bstrap <- c()
for (i in 1:1000){
  bstrap <- c(bstrap, mean(sample(tgt,cnt,replace=T)))
}
ql <- quantile(bstrap,.025) 
qh <- quantile(bstrap,.975)

### output the result to console:
cat("confidence interval is: ",ql," - ",qh)