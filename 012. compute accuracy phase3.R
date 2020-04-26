## file to analyse phase 2

printfile<-readline("Do you want to save a new file with the parameters [y/n]")


cd<-getwd()
setwd("merged_phases")
phase3files<-list.files(pattern = ".*csv")
setwd(cd)
#initialise matrix
Perf<-matrix(NA, nrow = length(phase3files),ncol = 8) 
colnames(Perf)<-c("PartNum","perfscene4lowPE", "perfscene4highPE","perfscene5lowPE", "perfscene5highPE",
                  "perfscene6lowPE","perfscene6lhighPE", "perfmediumPE")
                
# compute recognition performance
for (j in 1 :  length(phase3files)){
  # initialise the count for the column
  coln<-2
  file<-read.csv(paste("merged_phases/",phase3files[j], sep=""))
  for (s in 4:6){ # we are looping through scenes
    #subset only trials for scene s and only for old item (so we are looking only at correct recogntion, not rejection)
    # the variable indicating the scene is "pred_Cont"
    fileSub<-file[file$rec_trialType=="old" & file$pred_Cont==s,]
    #loop through the three conditions (low, medium, high PE)
    cond<-c(2,3)
    for (c in 1:2){
      filePE<-fileSub[fileSub$pred_Cond==cond[c],]
      acc<-mean(filePE$rec_acc)
      #we need two lines to adjust for participants' name, which is one digit only up to 10
      Perf[j, c(1,coln)]<-c(round(as.numeric(substr(phase3files[j],5,6)),1), acc)
      coln<-coln+1
    }
  }
  #now medium PE
  filePE<-file[(file$rec_trialType=="old")& (file$pred_Cont==1 |file$pred_Cont==2|file$pred_Cont==3),]
  acc<-mean(filePE$rec_acc)
  Perf[j, c(1,coln)]<-c(round(as.numeric(substr(phase3files[j],5,6)),1), acc)
}
#save it
dataframe<-data.frame(Perf)
if (printfile=="y"){
  write.csv(dataframe, "/home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational models/RW, Bayes, and PREMUP pilot/Results Phase3.csv")
}
