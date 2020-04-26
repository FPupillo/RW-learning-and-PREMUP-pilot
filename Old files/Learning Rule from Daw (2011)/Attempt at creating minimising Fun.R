# new script to find the maximum likelihood and find alpha and beta parameters, according to Daw (2011)

# "Fri Apr 10 10:07:50 2020"

# first, create the function

FINDalphabeta<-function(alpha, beta){
  #set the path
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
  
  #retrieve the file
  file<-read.csv(phase1Files[1])
  #subset only trials for scene 1. 
  fileSub<-file[file$scene_cat==4,]
  
  # at each trial, participants assign value to each category (1,2,3). 
  # we initialise the values for each category
  Q1<-0
  Q2<-0
  Q3<-0
  
  # initialise the reinforcement
  # the reinforcement depends on whether that trial is correct or not
  Qr<-NULL
  for (r in 1 :length(fileSub$scene_cat)){
    if (fileSub$acc[r]==1){
      Qr[r]<-1}else{Qr[r]<-0}
  }
  # apply the learning rate formula by updating it at every trial
  # initializa the predition error as delta
  delta1<-0
  delta2<-0
  delta3<-0
  
  # we also need three counters
  count1<-1
  count2<-1
  count3<-1
  
  #finally, we need variables for the implementation of the choice
  choice1<-NULL
  choice2<-NULL
  choice3<-NULL
  
  # take the log of the choice
  choice1log<-NULL#
  for (t in 1: length(fileSub$scene_cat)){
    
    if (fileSub$object_cat[t]==1){
      # delta, the prediction error, is the reinforced at trial t minus the value of the previous trial
      delta1[count1]<-(Qr[t]-Q1[count1])
      
      # then, on each trial, the value is updated as Qt+1(ct) = Qt(ct) + α · δ
      Q1[count1+1]<- Q1[count1]+ alpha* delta1[count1]
      fileSub$PE[t]<-abs(delta1[count1])
      count1<-count1+1
      
    } else if (fileSub$object_cat[t]==2){
      delta2[count2]<-(Qr[t]-Q2[count2])
      Q2[count2+1]<- Q2[count2]+ alpha* delta2[count2]
      fileSub$PE[t]<-abs(delta2[count2])
      count2<-count2+1
    } else {
      delta3[count3]<-(Qr[t]-Q2[count3])
      Q3[count3+1]<- Q3[count3]+ alpha* delta3[count3]
      fileSub$PE[t]<-abs(delta3[count3])
      count3<-count3+1
    }
    
    
    # this was the learning model, now we need an observation model. In reinforcement learning
    # it is assumed that subjects choose probabilistically according to a softmax distribution
    
    choice1[t]<-exp(beta *tail(Q1,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1))) #the tail functions determines the last value of the array
    choice2[t]<-exp(beta *tail(Q2,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))
    choice3[t]<-exp(beta *tail(Q3,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))
    
    choice1log[t]<-beta *tail(Q1,1)-log(exp(beta*tail(Q1,1)))+exp(tail(Q2,1))+exp(beta *tail(Q3,1))
  }
  return(sum(choice1log))
}
  

install.packages("pracma")
install.packages("NlcOptim")
library(pracma)
x0<-(0.3,5)
fminsearch(FINDalphabeta,x0)
fmincon(x0, FINDalphabeta)

        