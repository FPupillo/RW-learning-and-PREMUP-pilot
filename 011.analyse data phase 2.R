#################################################################################
# is there a correlation between the alpha and absPE and the percent participants choce the category mostly associated with a cue 
# in phase 2? 
rm (list=ls())
# retrieve the files for phase two
data<-read.csv("participants' perf Phase2.csv")
#aggregate performance for each scene
data$perf<-rowMeans(data[,c(3:5)])

# start with the R squared
dataframe<-dataframeRsquared()
# attach preformance phase 2
dataframe$perfphase2<-data$perf
# regress it
modelPhase2<-lm(perfphase2~alpha*absPE, data=dataframe)
summary(modelPhase2)
#drop the interaction
modelPhase2noINT<-lm(perfphase2~alpha+absPE, data=dataframe)
summary(modelPhase2noINT)


# now use the ML data
dataframe<-dataframeML()
# attach preformance phase 2
dataframe$perfphase2<-data$perf
# regress it
modelPhase2<-lm(perfphase2~alpha*absPE, data=dataframe)
summary(modelPhase2)
#drop the interaction
modelPhase2noINT<-lm(perfphase2~alpha+absPE, data=dataframe)
summary(modelPhase2noINT)
