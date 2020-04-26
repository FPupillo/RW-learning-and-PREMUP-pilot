# analyse data phase 3
# retrieve the file
data<-read.csv("Results Phase3.csv")

# aggregate perf at participant level, for high and low pE
data$highPE<-rowMeans(data[,c(3,5,7)])
data$lowPE<-rowMeans(data[,c(4,6,8)])

#plot it
# reshope it
library(reshape2)
datalong<-data[,c(2, 9,10,11)]
datalong<-melt(datalong, id = "PartNum" )
datalong$variable<-relevel(datalong$variable, ref="lowPE")
colnames(datalong)[2]<-"PE"
library(ggplot2)#
library(Hmisc)
ggplot(datalong, aes(PE, value))+ geom_bar(aes(PE, value, fill = PE),
    position="dodge",stat="summary", fun.y="mean", SE=T)+
stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
geom_jitter( size=1,width=0.1)

# do the alpha and abs PE correlate with performance?
# retrieve functions
source("007. analysesPhase1.R")
# remove unnecessary things
rm(list=ls()[c(1,4:10)])

# retrieve dataframe r squared
dataframe<-dataframeRsquared()
# attach performance phase 3
dataframe$lowPE<-data$lowPE[1:29]
dataframe$highPE<-data$highPE[1:29]
dataframe$mediumPE<-data$perfmediumPE[1:29]
# create aggregated performance
dataframe$accAll<-rowMeans(dataframe())
# select only alpha, beta, and absPE
dataframe<-dataframe[,c(2, 18:25)]
cor(dataframe)

newdata<-dataframe[, c(1,2,6,7,8,9)]
datalong<-melt(newdata, id = c("PartNum", "alpha", "absPE" ))

# plot alpha
g<-ggplot(datalong, aes( alpha, value, variable))
g+facet_wrap(~variable)+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

# plot absPE
g<-ggplot(datalong, aes( absPE, value, variable))
g+facet_wrap(~variable)+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
# now we want to see whether there is an interaction between alpha and abs(PE)

# let's do it with the data from ML
# retrieve dataframe ML
dataframe<-dataframeML()
# attach performance phase 3
dataframe$lowPE<-data$lowPE[1:29]
dataframe$highPE<-data$highPE[1:29]
dataframe$mediumPE<-data$perfmediumPE[1:29]
# create aggregated performance
dataframe$accAll<-rowMeans(dataframe[,19:21])
# select only alpha, beta, and absPE
dataframe<-dataframe[,c(2, 15, 18,19,20, 21,22)]
cor(dataframe)

newdata<-dataframe[, c(1:6)]
datalong<-melt(newdata, id = c("PartNum", "alpha", "absPE" ))
colnames(datalong)[c(4:5)]<-c("PE", "acc")
datalong$PE<- ordered(datalong$PE, levels = c("lowPE", "mediumPE", "highPE"))
# plot alpha
g<-ggplot(datalong, aes( alpha, acc, PE))
g+facet_wrap(~PE)+
  geom_point()+
  geom_smooth(method = "lm", se = T)

# plot absPE
g<-ggplot(datalong, aes( absPE, acc, PE))
g+facet_wrap(~PE)+
  geom_point()+
  geom_smooth(method = "lm", se = T)

# rm outlier
out<-which(datalong$absPE>0.60)
datalong<-datalong[-c(out),]

# replot
# plot absPE
g<-ggplot(datalong, aes( absPE, value, variable))
g+facet_wrap(~variable)+
  geom_point()+
  geom_smooth(method = "lm", se = T)

# now analyze
library(ez)
# effect of PE condition
modelPE<-ezANOVA(data	=	datalong,	dv	=	.(acc),	wid	=	.
                   (PartNum),	within	=	.(PE),	detailed	=	TRUE,	type	=	3)
modelPE

#effect of alpha
library (lme4)
library(lmerTest)
library(car)
modelalpha<-lmer(acc~alpha*PE+(1|PartNum), data= datalong)
summary(modelalpha)
Anova(modelalpha)
# not significant

modelPEabs<-lmer(acc~absPE*PE+(1|PartNum), data= datalong)
summary(modelPEabs)
Anova(modelPEabs)

