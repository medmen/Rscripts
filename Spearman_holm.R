#!/usr/bin/Rscript --slave
#####
# how to use
#####
usage <- "usage: Rscript spearman_holm.R <file-with-input-data.csv>"

###################
## set options here if needed
##################

options(OutDec= ",") # decimals are separated by komma (European style)
outdir <- "sig_graph" # name of subdirectory for storing plots

######### NO more options below here #####


### handle command line arguments (i.e. name of data file)
args <- commandArgs(TRUE)
inputfile <- args[1]

### open file, rad.csv2 expects fields separated by semicolon (";") and the komma (",") as decimal separator
incomingData <- read.csv2(inputfile, header=TRUE, na="NA", check.names=FALSE)

headers <- names(incomingData)

attach(incomingData)

# install.packages("psych")
library("psych")

ct <- corr.test(incomingData, method="spearman", adjust="holm")

cplot <- cor.plot(incomingData,numbers=TRUE,colors=TRUE,main=NULL,show.legend=TRUE,labels=NULL,select=1:5)

ci.data <- cor.ci(incomingData, n.iter = 100,  p = 0.05, method = "spearman")
ci.ci <- cor.plot.upperLowerCi(ci.data, main="Konfidenzintervalle der Korrelationen")  #to show the upper and lower confidence intervals
ci.ci

###
## write shortened CSV-File with results
###
r <- ct$r
n <- ct$n
P <- ct$p

r[n<5]<-NA # ignore less than five observations
# r[r>-0.5 & r<0.5]<-NA # take only "good" korrelations
P[is.na(r)]<-NA # delete P values for deleted korrelations 

###
## store plots in a subdirectory
## check if this subdirectory exists or create it
###
ifelse(!dir.exists(file.path(getwd(),outdir)), dir.create(file.path(getwd(),outdir)), FALSE)

###
## create scatterplots for all variables with significant correlation
###

#helper functions for char matching
starts_with <- function(needle, haystack) {
  return(substr(haystack,1,nchar(needle)) == needle)
}

datamatrix <- data.matrix(incomingData)
varnames <- dimnames(datamatrix)[[2]]
nvar <- length(varnames)
# for loops through all possible pairs
for(j in 1:(nvar-1)) {
  for(i in (j+1):nvar) {
    # get short variables to ease reading
    r <- ct$r[i, j]
    n <- ct$n[i, j]
    P <- ct$p[i, j]
    ### conditions necessary for saving graph:
    # - more than 5 observations
    # - significance level is met
    if(n >= 5 
       & (P <= 0.05) 
    ) {
      # to save as pdf: pdf(paste0("sig_graph/",varnames[j], "_vs_", varnames[i], ".pdf"))
      setEPS()
      postscript(paste0("sig_graph/",varnames[j], "_vs_", varnames[i], ".eps"))
      plot(datamatrix[, j], datamatrix[, i], xlab=varnames[j], ylab=varnames[i],
           # main=paste0("R = ", format(round(r, 2), nsmall=2), ", P = ", format(round(P, 2), nsmall=4)))
           main=paste0("R = ", format(round(r, 2), nsmall=2)))
      abline(lsfit(datamatrix[, j], datamatrix[, i]))
      dev.off()
    }
  }
}

###
## write results to csv files
### 
outfile_csv_R <- paste0(inputfile,'_R_spearman.csv')
outfile_csv_p <- paste0(inputfile,'_p_spearman.csv')
outfile_csv_ci <- paste0(inputfile,'_ci_spearman.csv')
# write to files
write.csv(r, file = outfile_csv_R, na="")
write.csv(P, file = outfile_csv_p, na="")
write.csv(ci.ci, file = outfile_csv_ci, na="")

###
## write combined results to a latex table in a separate file, add significance indicators
###
outfile_tex <- paste0(inputfile,'_spearman_.tex')

### output to LateX
# install.packages("xtable")
library("xtable")
# define notions for significance levels; spacing is important.
mystars <- ifelse(P == 0, "", ifelse(P < .001, "***", ifelse(P < .01, "** ", ifelse(P < .05, "*  ", "  "))))

r <- format(round(cbind(rep(-1.11, ncol(r)), r), 2))[,-1] ## trunctuate matrix with correlations to 2 decimals
P <- format(round(cbind(rep(-1.11, ncol(P)), P), 3))[,-1] ## trunctuate matrix with P-Values to 3 decimals
c <- format(round(cbind(rep(-1.11, ncol(ci.ci)), ci.ci), 2))[,-1] ## trunctuate matrix with P-Values to 2 decimals

combined <- matrix(paste(r, mystars, sep="newline"), ncol=ncol(r))

rownames(combined) <- rownames(r)
colnames(combined) <- colnames(r)

latex <- xtable(combined)
print(latex,file = outfile_tex, na="")

### clean up
detach(incomingData)

