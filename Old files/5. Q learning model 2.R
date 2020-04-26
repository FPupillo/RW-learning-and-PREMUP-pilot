#new Q learning model with lambda set at 0.80 and 0.10 depending on the trials

rm(list=ls())

#the categories trial after trial are the following
objCat<-c(1 ,1 ,1 ,2 ,1 ,1 ,3 ,1, 1, 1, 1, 3, 1, 1 ,1 ,1 ,2 ,1 ,1 ,1)
#set the path
if (Sys.info()[1]=="Linux"){
  setwd("/home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational model/PIVOTAL/pilot_study/trial_sequences")
} else setwd("/home/Users/francescopupillo/PowerFolders/Frankfurt_University/Computational model/PIVOTAL/pilot_study/trial_sequences")

#select only phas1e
phase1Files<-NULL
for (i in 1: length (list.files())){
  check<-substr((list.files()[i]), 8,13)
  if(check=="phase1"){
    phase1Files<- c(phase1Files,(list.files()[i]))
  }
}

alpha<-0.3 #alpha is the learning rate linked to first stimulus
n_trials <- 20  
strength <- numeric(n_trials)
##now load participants' data and loop through participants


#loop through participants
alphaTot<-NULL
par(mfrow=c(5,7))
for (j in 1 :  length(phase1Files)){
  alpha<-0.3 #alpha is the learning rate linked to first stimulus
  file<-read.csv(phase1Files[j])
  #subset only trials for scene 1. 
  fileSub<-file[file$scene_cat==4,]
  
  choice<-NULL# actual choise made by participants
  performance<-NULL   #actual performance by participant, as the sum of the expected choice over the sum of the trials
  for (e in 1 :length(fileSub$scene_cat)){
    if(fileSub$response[e]==1){choice[e]<-1
    }else{choice[e]<-0}
    performance[e]<-sum(choice)/e
  } 
  
  #apply the rescorla-wagner algorithm at each trial
  Qfunction<-function(alpha, strength){
    lambda<-NULL
    prediction_error<-NULL
    
    for (l in 1: length(objCat)){
      if(objCat[l]==1){
        lambda[l]<-0.80
      }else{lambda[l]<-0.10}
    }
    
    for(i in 2:n_trials) {
      
      value_compound <- sum(strength[i-1])
      prediction_error[i] <- lambda[i] - value_compound
      #here we should change the beta, the learning of the second category
      #beta = betaTr[i-1]
      value_change <- alpha  * prediction_error[i] 
      #omly with beta
      #value_change <- beta * prediction_error[i] 
      
      strength[i] <- sum(strength[i-1]) + value_change
      
      #implementing the softmax rule. 
      #prob[i]<-exp(strength[i])/alpha)/exp(strength[i]/alpha)+exp()
      
    }
    my_list<-list(strength,prediction_error)
    
    return(my_list)}
  
  strength<-Qfunction( alpha, strength)[[1]]
  prediction_error<-Qfunction( alpha, strength)[[2]]
  
  #sum of squared residuals: sum of empirical - thoeretical deviations squared
  SSR<-sum((performance-strength)^2)
  #sum of squared total: sum of empirical - mean, squared
  SST<-sum((performance-mean(performance))^2)
  #SSM: sum of square of the model
  SSM<-SST-SSR
  Rsquared<-SSM/SST
  
  ###try to change the learning rates
  Rsquared1<-c(Rsquared, Rsquared)
  m<-1 #counter
  while(Rsquared1[m+1]>=Rsquared1[m]){
    strength<- numeric(n_trials)
    alpha<-alpha+0.01
    strength<-Qfunction(alpha,strength)[[1]]
    prediction_error<-Qfunction( alpha, strength)[[2]]
    
    SSR<-sum((performance-strength)^2)
    SST<-sum((performance-mean(performance))^2)
    SSM<-SST-SSR
    Rsquared1[m+2]<-SSM/SST
    m<-m+1
  }
  Rsquared1<-c(Rsquared, Rsquared)
  m<-1
  while(Rsquared1[m+1]>=Rsquared1[m]){
    strength<- numeric(n_trials)
    alpha<-as.numeric(alpha)-0.01
    strength<-Qfunction(alpha, strength)[[1]]
    prediction_error<-Qfunction( alpha, strength)[[2]]
    
    SSR<-sum((performance-strength)^2)
    SST<-sum((performance-mean(performance))^2)
    SSM<-SST-SSR
    Rsquared1[m+2]<-SSM/SST
    m<-m+1
  }
  
  alphaTot[j]<-alpha
  plot(performance, type="l", col="black", ylim=c(0,1), main = paste(substr(phase1Files[j],5,6),", LR = ",round(alphaTot[j],2), sep="" ),xaxt='n',
       xlab="Trials")
  lines(strength, col="red")
  lines(prediction_error, col="blue")
  
  #legend( 1,1,legend=c("Empirical", "Theoretical"),
  # cex=0.3, lty = c(1,1), col = c(1,2))
  axis(1, at = c(1:length(objCat)),  labels = objCat)
  
}
par(mfrow=c(1,1))
