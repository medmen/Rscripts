#!/usr/bin/Rscript --slave
#####
# how to use
#####
usage <- "usage: Rscript ks_normality.R <file-with-input-data.csv>"

###
#
# TOOO: add p value in graph legend
###

### handle command line arguments (i.e. name of data file)
args <- commandArgs(TRUE)
inputfile <- args[1]

# open file, rad.csv expects fields separated by komma (",") and the dot (".") as decimal separator (US)
# open file, rad.csv2 expects fields separated by semikolon (";") and the komma (",") as decimal separator (European)

#incomingData <- read.csv2(inputfile, header=TRUE, check.names=FALSE)
incomingData <- read.csv2(inputfile, header=TRUE)
# give names of headers
headers <- names(incomingData)

# attach datafile so we can refer to variable names directly
# don't forget to use detach at the end
attach(incomingData)

# use plyr for colwise test
#install.packages("plyr") 
library("plyr")

### TEST functions ###
### 1.) Kolmogorov-Smirnov-Test
### input: x = data, y: pnorm for normal distribution, plnorm for lognormal distribution
ks_test <- function(x,y="pnorm") {
	mu <- mean(x)
	sigma <- sd(x)
	foo <- ks.test(x=x, y=y, mean = mu, sd = sigma)
	sim <- foo$statistic
	return(sim)
}

shapiro_test <- function(x) {
	shapiro <- shapiro.test(x)
}

### END TEST functions ###

# Apply to every column in a data frame
normals <- numcolwise(shapiro_test)(incomingData)

# use psych for calculation of SD and MEAN
library("psych")
# use ggplot2 for printing plots
library("ggplot2")

# for loops through all columns
nvar <- length(headers)
### summary gives descriptive stats for each column
# summary(incomingData)

for(i in 1:(nvar)) {
	nam <- headers[i]
  ### calculate mean and sd for normal curve
	mean <- summary(incomingData[[nam]])[4]
	sd <- SD(incomingData[[nam]])
  ### calculate min and max as well as number of valid data points for binwidth
  min <- summary(incomingData[[nam]])[1]
  max <- summary(incomingData[[nam]])[6]
	num_nas <- summary(incomingData[[nam]])[7]
	if(is.na(num_nas)) {
	  num_nas <- 0
	}
  valid_datapoints <- length(incomingData[[nam]]) - num_nas
  binwidth <- (max - min) / valid_datapoints
	cat("nam: ", nam, "max: ",max,"\n min: ",min,"\n valid samples: ",valid_datapoints,"\n binwidth: ", binwidth, "\n\n")

  if(is.na(binwidth)) {
    cat("COUGHT NA FOR VARIABLE ",nam,".. will SKIP processing this!!\n\n")
    next
  }
  
	filename <- paste0("sig_graph/",nam,"_dichte_vs_normalverteilung.png")
	ggplot(incomingData, aes_string(x=nam)) + 
		geom_histogram(aes(y = ..density..), alpha=0.2, binwidth=binwidth, fill="red",colour="black") + 
		stat_function(fun=dnorm, args=list(mean=mean, sd=sd), colour="green", label="Normalverteilung") 
		# stat_function(fun=dlnorm, args=list(mean=mean, sd=sd), colour="green", label="Lognormale Verteilung") +
		theme(legend.position = "bottom", legend.direction = "horizontal")
	ggsave(file=filename)
  
  qqfilename <- paste0("sig_graph/",nam,"qq-plot.png")
  png(qqfilename)
	qqnorm(incomingData[[nam]],
	       main=paste0("Normalverteilung Q-Q Plot für ", nam),
	       xlab=paste0("theoretische Quantilen von ", nam),
	       ylab=paste0("gemessene Quantilen von ", nam))
	qqline(incomingData[[nam]])
	dev.off()
}

warnings()


# form output file names
 csvnorm <- paste0('shapiro_normality_',inputfile)
# write to files
write.csv2(normals, file = csvnorm)


### output to LateX
# install.packages("xtable")
library("xtable")
latex <- xtable(normals)
# latex <- xtable(is_uniform)
print(latex,file=paste0(csvnorm,".tex"))

### clean up
detach(incomingData)
