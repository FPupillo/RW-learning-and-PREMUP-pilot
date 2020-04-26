# estimate for the scene of medium PE (1,2,3) whether participants chose preferibly one category (1,2,3)

  printfile<-readline("Do you want to save a new file with the parameters [y/n]")
  # config files with functions
  source("004. Config and Parameter estimation function.R")
  
  # select only phase 1
  cd<-getwd() # as the following function change the directory, we need to store it and recall it after callin ght function
  phase1Files<-selPhase(1)
  setwd(cd)
  
  # initialize matrix to store the choice
  alldata<-matrix(NA, nrow = length(phase1Files),ncol = 10) 
  colnames(alldata)<-c("PartNum", "scene1-1", "scene1-2", "scene1-3","scene2-1", "scene2-2", "scene2-3","scene3-1", "scene3-2", "scene3-3" )
  
  # loop through participants
  for (j in 1 :  length(phase1Files)){
    coln<-2 #counter for the column number of the last file
    file<-read.csv(paste("trial_sequences/",phase1Files[j], sep=""))
    alldata[j,1]<-round(as.numeric(substr(phase1Files[j],5,6)),1)
    # loop thorugh scenes
    for (s in 1:3){ 
      fileSub<-file[file$scene_cat==s,] 
      # initialize variable "choice" and preference
      choice<-NULL
      preference<-NULL
      # loop through categories
      for (c in 1:3){
        for (e in 1 :length(fileSub$scene_cat)){
          if(fileSub$response[e]==c){choice[e]<-1
          }else{choice[e]<-0}
          preference[e]<-sum(choice)/e
        } 
        alldata[j,coln]<-tail(preference,1)
       coln<-coln+1 
      }
    }
  }

  dataframe<-as.data.frame(alldata)
  if (printfile=="y"){
    write.csv(dataframe, "/home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational models/RW, Bayes, and PREMUP pilot/choice123.csv")
  }
  IND<-which(alldata[,2:10] > 0.667, arr.ind=T)
  ind<-IND[,2]+1
  IND[,1]<-alldata[,1][IND[,1]]
  IND[,2]<-colnames(alldata)[ind]
  
  # sort by participant
  IND<-IND[order(IND[,1]),]
  return(IND)
    
    