# finding parameters with R2 from learning rule (Daw, 2011)


rm(list=ls())
# set the path
if (Sys.info()[1]=="Linux"){
  setwd("/home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational model/PIVOTAL/pilot_study/trial_sequences")
} else setwd("/Users/francescopupillo/PowerFolders/Frankfurt_University/Computational model/PIVOTAL/pilot_study/trial_sequences")

#select only phase1
phase1Files<-NULL
for (i in 1: length (list.files())){
  check<-substr((list.files()[i]), 8,13)
  if(check=="phase1"){
    phase1Files<- c(phase1Files,(list.files()[i]))
  }
}

#retrieve the functions we need
source(if (Sys.info()[1]=="Linux"){
  ("/home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational model/Learning Rule from Daw (2011)/Parameter estimation function.R")
} else ("/Users/francescopupillo/PowerFolders/Frankfurt_University/Computational model/Learning Rule from Daw (2011)/Parameter estimation function.R")
)

Parameters<-matrix(NA, nrow = length(phase1Files),ncol = 10) #matrix to store the parameters
colnames(Parameters)<-c("PartNum", "alpha4", "beta4","perf4", "alpha5", "beta5", "perf5","alpha6","beta6",  "perf6") #names of the columns


par(mfrow=c(5,3))
par(mar = c(4,2,2,1))

# loop through participants
for (j in 1 :  length(phase1Files)){
#do it for all the scenes
  
  coln<-2 #counter for the column number of the last file
  
  file<-read.csv(phase1Files[j])
  
  for (s in 4:6){ # loop through scenes
  #subset only trials for scene 1. 
  fileSub<-file[file$scene_cat==s,]
  
  choice<-NULL# actual choise made by participants
  preference<-NULL   #actual performance by participant, as the sum of the expected choice over the sum of the trials
  if (s==4){c<-1
  } else if (s==5){c<-2
  } else if (s==6){c<-3}
  
  for (e in 1 :length(fileSub$scene_cat)){
    if(fileSub$response[e]==c){choice[e]<-1
    }else{choice[e]<-0}
    preference[e]<-sum(choice)/e
  } 
  
  Par<-findPar(preference,c)
  
  Parameters[j,c(1,coln,coln+1,coln+2)]<-c(round(as.numeric(substr(phase1Files[j],5,6)),1),round(Par[[1]],2), round(Par[[2]],2), preference[20])
  
  coln<-coln+3
  
  Prob<-findProb(Par[[1]],Par[[2]])[[c]]
  PE<-findProb(Par[[1]],Par[[2]])[[4]] #unsigned PE
  # plot
  plot(preference, ylim=c(-1,1), xlim=c(1, 20),xaxt="n",xlab="",ylab="Preference for Category")
  lines(Prob)
  lines(PE, col="red")
  #abline(h=c(0.,0.2,0.4,0.6,0.8), lty=1, col="grey")#for signed PE
  abline(h=c(-0.2,-0.4,-0.6,-0.80,0.2,0.4,0.6,0.8), lty=1, col="grey")#for unsined PE
  abline(h=0, lty=1, col="black")#for the zero
  abline(v=seq(1:20), lty=1, col="grey")#
  #axis(3, at = c(1:length(fileSub$scene_cat)),  labels = fileSub$object_cat)
   #mtext("Obj cat", 3, line=1,at =-3)
   axis(1, at = c(1:length(fileSub$scene_cat)),  labels = fileSub$acc)
   mtext("Accuracy", 1, line=2)
  title(main = paste("alpha = ", round(Par[[1]],2), ",","beta = ",round(Par[[2]],2)), line=0.1)
  title(main=paste("Part = ", substr(phase1Files[j],5,6),"scene = ", s), line=1)
  
}
  
}
par(mfrow=c(1,1))

dataframe<-as.data.frame(Parameters)
#write.csv(dataframe, "/home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational model/Learning Rule from Daw (2011)/participants' parameters estimate.csv")


# compute aggregated alpha and beta
# dataframe$alpha<-rowMeans(dataframe[,c(2,5,8)])
# dataframe$beta<-rowMeans(dataframe[,c(3,6,9)])
# dataframe$perf<-rowMeans(dataframe[,c(4,7,10)])
# 
# model<-lm(perf~alpha+beta, data=dataframe)
# summary(model)
# 
# plot(dataframe$perf~ dataframe$beta)
# abline(lm(dataframe$perf~dataframe$beta))

library(ggplot2)

a<-ggplot(dataframe, aes(alpha, perf))
a+ geom_point()+ geom_smooth(method="lm")
    