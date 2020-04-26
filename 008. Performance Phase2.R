##  performance for phase2

#parameterEstimationPhase1maximumLikel<-function(){
printfile<-readline("Do you want to save a new file with the parameters [y/n]")
printplot<-readline("Would you like to print the plots? [y/n]")  

# retrieve functions
source("004. Config and Parameter estimation function.R")
cd<-getwd() # as the following function change the directory, we need to store it and recall it after callin ght function
phase2Files<-selPhase(2)
setwd(cd)

# initialize variables
choice<-NULL
preference<-NULL
Pref<-matrix(NA, nrow = length(phase2Files), ncol=4)
colnames(Pref)<-c("PartNum","perf4",  "perf5",
                         "perf6")
if (printplot=="y"){
  # Open a pdf file
  pdf("Phase2perf.rplot.pdf")
}
# set the margins for the graphs
par(mfrow=c(5,3))
par(mar = c(4,2,2,1))
# compute performance (perference for the most likely category)
for (j in 1 :  length(phase2Files)){
  coln<-2 #counter for the column number of the last file
  # read the files
  file<-read.csv(paste("trial_sequences/",phase2Files[j], sep=""))
  for (s in 4:6){ # we are looping through scenes
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
    
    Pref[j, c(1,coln)]<-c(round(as.numeric(substr(phase2Files[j],5,6)),1), tail(preference,1))
    
    coln<-coln+1
    # plot
    plot(preference, ylim=c(0,1), xlim=c(1, 80),xaxt="n",xlab="",ylab="Preference for Category")
    axis(1, at = c(1:length(fileSub$scene_cat)),  labels = seq(1,length(fileSub$scene_cat)))
    
    title(main=paste("Part = ", substr(phase2Files[j],5,6),"scene = ", s), line=1)
  }
}

if (printplot=="y"){
  # Close the pdf file
  dev.off()
}
par(mfrow=c(1,1))
setwd(cd)        

#convert the matrix into a dataframe object
dataframe<-as.data.frame(Pref)

#save it
if (printfile=="y"){
  write.csv(dataframe, "/home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational models/RW, Bayes, and PREMUP pilot/participants' perf Phase2.csv")
}