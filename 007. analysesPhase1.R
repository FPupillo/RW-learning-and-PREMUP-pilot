# analyse parameters and performance from Q-learning model
#####
rm(list=ls())
#retrieve data
# first from the r squared 
dataframeRsquared<-function(){
dataframe<-read.csv("participants' parameters estimate.csv")
# compute aggregated alpha, beta, and PE
dataframe$alpha<-rowMeans(dataframe[,c(3,8,13)])
dataframe$beta<-rowMeans(dataframe[,c(4,9,14)])
dataframe$perf<-rowMeans(dataframe[,c(5,10,15)])
dataframe$PE<-rowMeans(dataframe[,c(6,11,16)])
dataframe$absPE<-rowMeans(dataframe[,c(7,11,17)])
return(dataframe)
}

dataframe<-dataframeRsquared()
model<-lm(perf~alpha*absPE*beta, data=dataframe)
step(model)

#final model
final<-lm(formula = perf ~ alpha + beta + absPE + alpha:beta + beta:absPE, 
   data = dataframe)
summary(final)

# let's try the absolute PE
model2<-lm(perf~alpha+beta+absPE, data=dataframe)
summary(model2)

# drop the alpha
model3<- lm(perf~beta+absPE, data=dataframe)
summary(model3)

# plot it
# first center it
dataframe$betasc<-scale(dataframe$beta)
dataframe$absPEsc<-scale(dataframe$absPE)

plot(dataframe$betasc, dataframe$perf, col="red")
abline(lm(dataframe$perf~dataframe$betasc+dataframe$absPEsc), col="red")
points(dataframe$perf~dataframe$absPEsc, col="green")
abline(lm(dataframe$perf~dataframe$absPEsc+dataframe$betasc), col="green")

# now analyze data from the maximum likelihood extimation
dataframeML<-function(){
dataframe<-read.csv("participants' parameters estimate max likel.csv")
# compute aggregated alpha, , and PE
dataframe$alpha<-rowMeans(dataframe[,c(3,8,13)])
dataframe$perf<-rowMeans(dataframe[,c(5,10,15)])
dataframe$PE<-rowMeans(dataframe[,c(6,11,16)])
dataframe$absPE<-rowMeans(dataframe[,c(7,11,17)])
return(dataframe)
}

dataframe<-dataframeML()
model<-lm(perf~alpha*absPE, data=dataframe)
summary(model)

#drop the interaction
model2<-lm(perf~alpha+absPE, data=dataframe)
summary(model2)

#plot
plot(dataframe$perf~dataframe$alpha)
abline(lm(dataframe$perf~dataframe$alpha), col="red")
abline(lm(dataframe$perf~dataframe$absPE), col="green")

library(ggplot2)
p<-ggplot(dataframe, aes(alpha, perf))
p+geom_point()+
  geom_smooth(method = "lm", se = T)

q<-ggplot(dataframe, aes(absPE, perf))
q+geom_point()+
  geom_smooth(method = "lm", se = T)

# remove outlier
which(dataframe$absPE>0.80)
dataframe[14,]<-NA
q<-ggplot(dataframe, aes(absPE, perf))
q+geom_point()+
  geom_smooth(method = "lm", se = T)
