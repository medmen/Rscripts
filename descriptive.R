#!/usr/bin/Rscript --slave
#####
# how to use
#####
usage <- "usage: Rscript descriptive.R <file-with-input-data.csv>"

### handle command line arguments (i.e. name of data file)
args <- commandArgs(TRUE)
inputfile <- args[1]

# open file, rad.csv expects fields separated by komma (",") and the dot (".") as decimal separator
incomingData <- read.csv2(inputfile, header=TRUE, na="NA", check.names=FALSE)
# give names of headers
headers <- names(incomingData)

# attach datafile so we can refer to variable names directly
# don't forget to use detach at the end
attach(incomingData)

#install.packages("psych")
library("psych")

# describe gets following parameters for every column in a data frame
# item name ,item number, nvalid, mean, sd,
# median, mad, min, max, skew, kurtosis, se 
description <- describe(incomingData)

### an alternative is to describe by group like this
# grouped_description <- describe.by(mydata, group,...)

# form output file names
descr_file <- paste0('descriptive_',inputfile)
# write to files
write.csv(description, file = descr_file)

### output to LateX
library("xtable")
latex <- xtable(description)
print(latex,file=paste0(descr_file,".tex"))

### clean up
detach(incomingData)
