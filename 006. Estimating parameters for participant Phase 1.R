# finding parameters with R2 from learning rule (Daw, 2011)

# shall we save a file with the parameters?
printfile<-readline("Do you want to save a new file with the parameters [y/n]")
printplot<-readline("Would you like to print the plots? [y/n]")
# config files with functions
source("004. Config and Parameter estimation function.R")

# select only phase 1
cd<-getwd() # as the following function change the directory, we need to store it and recall it after callin ght function
phase1Files<-selPhase(1)
setwd(cd)

# initialize matrix to store the parameters
Parameters<-matrix(NA, nrow = length(phase1Files),ncol = 16) 
colnames(Parameters)<-c("PartNum", "alpha4", "beta4","perf4","PE4", "absPE4" ,"alpha5", "beta5", "perf5","PE5",
"absPE5","alpha6","beta6",  "perf6", "PE6", "absPE6") #names of the columns

# set the margins for the graphs
par(mfrow=c(5,3))
par(mar = c(4,2,2,1))

if (printplot=="y"){
  # Open a pdf file
  pdf("Phase1Rsquared.rplot.pdf")
}

# loop through participants
for (j in 1 :  length(phase1Files)){
#do it for all the scenes
  coln<-2 #counter for the column number of the last file
  # read the files
  file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
  # loop through scenes
  for (s in 4:6){ # we are o
  #subset only trials for scene s. 
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
  

  Prob<-findProb(Par[[1]],Par[[2]])[[c]]
  PE<-findProb(Par[[1]],Par[[2]])[[4]] #signed PE
  absPE<-findProb(Par[[1]],Par[[2]])[[5]] #signed PE
  
  Parameters[j, c(coln+3, coln+4)]<-c(round(mean(PE), 1), round(mean(absPE),1))
  coln<-coln+5
  # plot
  plot(preference, ylim=c(0,1), xlim=c(1, 20),xaxt="n",xlab="",ylab="Preference for Category")
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
  


if (printplot=="y"){
  # Close the pdf file
  dev.off()
}
par(mfrow=c(1,1))
setwd(cd)

dataframe<-as.data.frame(Parameters)
if (printfile=="y"){
write.csv(dataframe, "/home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational models/RW, Bayes, and PREMUP pilot/participants' parameters estimate.csv")
}


