#!/usr/bin/Rscript --slave
#####
# how to use
#####
usage <- "usage: Rscript regression.R <file-with-input-data.csv>"

### handle command line arguments (i.e. name of data file)
args <- commandArgs(TRUE)
inputfile <- args[1]
options(OutDec= ",")

data <- read.csv2("regressiondata.csv", header=TRUE, na="NA", check.names=FALSE)

# give names of headers
headers <- names(data)

# read dates as date
#data$birthyear <- as.Date(data$birthyear,format = "%m.%d.%Y")

# attach datafile so we can refer to variable names directly
# don't forget to use detach at the end
attach(data)

summary <- summary(data)
crosscorrelation <- cor(data[c(2,3,4,5,6,7,8)], use="complete", method="spearman")
paircorr_exfreq_het_MLD <- cor(data[c(2,6)], use="pairwise", method="spearman")
paircorr_exfreq_het_PK <- cor(data[c(2,7)], use="pairwise", method="spearman")
paircorr_exfreq_het_LAV2 <- cor(data[c(2,8)], use="pairwise", method="spearman")
# pairs(data) # pairwise graphics

model0 = lm(ExFreq ~ LAV2 + PI10 + PAA_Ratio + het_MLD + het_PK + het_LAV2, data=data)
summary(model0)
anova(model0)
ci <- confint(model0, level=0.95) # CIs for model parameters 
ci

par(mfrow=c(2,2))
plot(model0)

# Plot shows 1 extreme outlier (No 40), see what happens when we delete that
data2 <- subset(data, PID != 40)
summary <- summary(data2)
crosscorr2 <- cor(data2[c(2,3,4,5,6,7,8)], use="complete", method="spearman")
paircorr2_exfreq_het_MLD <- cor(data2[c(2,6)], use="pairwise", method="spearman")
paircorr2_exfreq_het_PK <- cor(data2[c(2,7)], use="pairwise", method="spearman")
paircorr2_exfreq_het_LAV2 <- cor(data2[c(2,8)], use="pairwise", method="spearman")

model1 = lm(ExFreq ~ LAV2 + PI10 + PAA_Ratio + het_MLD + het_PK + het_LAV2, data=data2)
summary(model1)
anova(model1)
par(mfrow=c(2,2))
plot(model1)

model2 = update(model1, .~.-het_MLD)
summary(model2)
md1 <- anova(model1, model2)
md1

model3 = update(model2, .~.-het_LAV2)
summary(model3)
md2 <- anova(model2, model3)
md2

model4 = update(model3, .~.-PI10)
summary(model4)
md3 <- anova(model3, model4)
md3


model5 = update(model4, .~.-PAA_Ratio)
summary(model5)
md4 <- anova(model4, model5)
md4

### clean up
detach(data)