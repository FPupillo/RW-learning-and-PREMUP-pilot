rm(list=ls())
#set.seed(11)
#priors expressed as alpha, the pseudo counts
alphas<-c(5,5,5)
#initialize variables
c<-as.numeric(c(NA,NA,NA))
#c<-c(5,3,1)
expected<-as.numeric(c(NA, NA, NA))
proportion<-NULL
choice<-NULL
performance<-NULL
#include the obhect category
objCat<-c(1 ,1 ,1 ,2 ,1 ,1 ,3 ,1, 1, 1, 1, 3, 1, 1 ,1 ,1 ,2 ,1 ,1 ,1)

par(mfrow=c(4,5))
par(mar = c(2, 2, 2, 2))
for (i in 1:20){
  
  #get the object category from the empirical trials  
  if (objCat[i]==1){
    c<-rbind(c,c(1,0,0))
  } else if ((objCat[i]==2)){
    c<-rbind(c,c(0,1,0))
  } else{c<-rbind(c,c(0,0,1))}
  
  observed<-which(c[i+1,]==1)
  #now calculate the expected
  expected<-rbind(expected,((alphas+colSums(c, na.rm=T))/(sum(colSums(c, na.rm=T))+sum(alphas,na.rm=T))))
  #update the alphas
  #alphas<-alphas+colSums(c, na.rm=T)
  alphas<-alphas+c[i+1,]
  #plot them
  barplot(expected[i+1,], ylim=c(0,1), xlab = paste("Trial N", i), main = paste("Observed", observed))
  #the choice is just a random sample from the posterior distribution
  choice[i]<-sample(c(1:3),p=expected[i+1,],1)
  performance[i]<-length(choice[choice==1])/i
}
par(mfrow=c(1,1))
plot(performance ,ylim=c(0,1), type="l")

#sample from the posterior
samples<-sample(c(1:3), p=expected[i+1,],1000, replace = T)

