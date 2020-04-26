###rescorla wagner model for associative learning implementation, fit to individual data. Estimate parameters
rm(list=ls())

#https://psyr.org/programming.html



#instead of having fixed alphas, 
#create an alpha that follows the sequence of the trials

#the categories trial after trial are the following
objCat<-c(1 ,1 ,1 ,2 ,1 ,1 ,3 ,1, 1, 1, 1, 3, 1, 1 ,1 ,1 ,2 ,1 ,1 ,1)
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

#create a variable to store the prediction error
prediction_error<-NULL
#set alpha at 0.80
alpha<-0.3 #alpha is the learning rate linked to first stimulus
beta<-0.3#beta is the learning rate linked to the category
lambda<-0.80 #lambda is the reward value, which is the maximum possible association strignth for the CS (in our case is 80%)


#So get started, we’ll specify the number of trials
n_trials <- 20  
strength <- numeric(n_trials)


# #apply the rescorla-wagner algorithm at each trial
# for(i in 2:n_trials) {
#   value_compound <- sum(strength[i-1])
#   prediction_error[i] <- lambda - value_compound
#   #here we should change the beta, the learning of the second category
#   beta = betaTr[i-1]
#   value_change <- alpha * beta * prediction_error[i] 
#   strength[i] <- sum(strength[i-1]) + value_change
# }
# plot(strength, ylim=c(0,1), type="l", col="red",
#      xlab="TrialN", ylab = "%", main = "Phase 1")
# lines(prediction_error, type="l")
# legend( 1,1,legend=c("Prediction Error", "Preference"),
#         cex=0.8, lty = c(1,1), col = c(1,2))
# axis(1, at = c(1:length(objCat)),  labels = objCat)

##now load participants' data and loop through participants




#loop through participants
betaTot<-NULL
par(mfrow=c(5,3))
par(mar = c(4,2,2,1))

for (j in 1 :  length(phase1Files)){
  #set alpha at 0.80
  alpha<-0.3 #alpha is the learning rate linked to first stimulus
  beta<-0.3#beta is the learning rate linked to the category
  lambda<-0.80 #lambda is the reward value, which is the maximum possible association strignth for the CS (in our case is 80%)
  #read file
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
  RWfunction<-function(alpha, beta, lambda, strength){
    betaTr<-NULL
    prediction_error<-NULL
    #insert omega
    omega<-1
    
    for (l in 1: length(objCat)){
      if(objCat[l]==1){
        betaTr[l]<-beta
      }else{betaTr[l]<--beta}
    }
  for(i in 2:n_trials) {
  
  
    value_compound <- sum(strength[i-1])
    prediction_error[i-1] <- lambda - value_compound
    #here we should change the beta, the learning of the second category
    beta = betaTr[i-1]
    value_change <- alpha  * beta*prediction_error[i-1] 
    #omly with beta
    #value_change <- beta * prediction_error[i] 
    
    strength[i] <- sum(strength[i-1]) + value_change
  }
    my_list<-list(strength,prediction_error, value_change)
    
  return(my_list)}
  
  strength<-RWfunction(alpha, beta, lambda,strength)[[1]]
  #plot it
  #lines(strength, type="l", col= "red")
  #now calculate the fit as R2. 
  #sum of squared residuals: sum of empirical - thoeretical deviations squared
  SSR<-sum((performance-strength)^2)
  #sum of squared total: sum of empirical - mean, squared 
  SST<-sum(performance-mean(performance)^2)
  #SSM: sum of square of the model
  SSM<-SST-SSR
  Rsquared<-SSM/SST

    ###try to change the learning rates
  Rsquared1<-c(Rsquared, Rsquared)
  
  
   # for (i in 1:100){
  m<-1 #counter
    while(Rsquared1[m+1]>=Rsquared1[m]){
    strength<- numeric(n_trials)
    beta<-beta+0.1
    strength<-RWfunction(alpha, beta, lambda,strength)[[1]]
    
    SSR<-sum((performance-strength)^2)
    SST<-sum(performance-mean(performance)^2)
    SSM<-SST-SSR
    Rsquared1[m+2]<-SSM/SST
    m<-m+1
    }
  
  while(Rsquared1[m+1]<=Rsquared1[m]){
    strength<- numeric(n_trials)
    beta<-beta-0.1
    strength<-RWfunction(alpha, beta, lambda,strength)[[1]]
    
    SSR<-sum((performance-strength)^2)
    SST<-sum(performance-mean(performance)^2)
    SSM<-SST-SSR
    Rsquared1[m+2]<-SSM/SST
    m<-m+1
  }
  
  prediction_error<-RWfunction(alpha, beta, lambda,strength)[[2]]
  #value_change<-RWfunction(alpha, beta, lambda,strength)[[3]]
  
  betaTot[j]<-beta
  plot(performance, type="l", col="black", ylim=c(0,1), main = paste(substr(phase1Files[j],5,6),", LR = ",betaTot[j], sep="" ),xaxt='n',
       xlab="Trials")
  lines(strength, col="red")
  lines(prediction_error, col="blue")
  
  #legend( 1,1,legend=c("Empirical", "Theoretical"),
         # cex=0.3, lty = c(1,1), col = c(1,2))
   #axis(1, at = c(1:length(objCat)),  labels = objCat)
  axis(1, at = c(1:length(fileSub$scene_cat)),  labels = fileSub$pe_level)
  
  
}
plot(performance, pch="", col="black", ylab="", xlab="")
legend( 0.8,0.5,legend=c("Empirical", "Theoretical", "Prediction error"),
        cex=0.9, lty = c(1,1,1), col = c(1,2,"blue"),text.width =10)
par(mfrow=c(1,1))


#write parameters
Data<-as.data.frame(cbind(Participants<-substr(phase1Files,5,6), Learning_rates<-betaTot))
names(Data)<-c("Participants", "Learning Rates")

#write.csv(Data, "/home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational model/PIVOTAL/pilot_study/RWM parameters.csv")


#correlate with the regression. 
#dataReg<-read.csv("ParticipantsParamPhase1.csv")
#dataRWM<-read.csv("RWM parameters.csv")

#cor.test(dataReg$Slope, dataEWM$Learning.Rates)
