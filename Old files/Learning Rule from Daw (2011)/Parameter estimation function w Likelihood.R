# function for estimating parameters 

#########################################################################################################
# function to extract the log likelihood
# it returns: 
# [[1]] Q1
# [[2]] Q2
# [[3]] Q3
# [[4]] choice1, 
# [[5]] choice2, 
# [[6]] choice3, 
# [[7]] prod(choice1log), 
# [[8]] prod(choice2log), 
# [[9]] prod(choice3log), 
# [[10]] PE,      #predition error
# [[11]] abs(PE) #absolute prediction error

findlikel<-function(alpha, beta){
  # initialise the reinforcement
  r<-NULL
  for (q in 1 :length(fileSub$scene_cat)){
    if (fileSub$acc[q]==1){
      r[q]<-1}else{r[q]<-0}
  }
  # initialize PE
  PE<-NULL
  # initialise the values for each category
  Q1<-0
  Q2<-0
  Q3<-0
  # initializa the prediction error as delta
  delta1<-0
  delta2<-0
  delta3<-0
  # we also need three counters
  count1<-1
  count2<-1
  count3<-1
  # we need variables for the implementation of the choice
  choice1<-NULL
  choice2<-NULL
  choice3<-NULL
  # we need variables to implement the log likelihood
  choice1log<-NULL
  choice2log<-NULL
  choice3log<-NULL
  # loop through participant's choice
  for (t in 1: length(fileSub$scene_cat)){
    
    if (fileSub$object_cat[t]==1){
      # delta, the prediction error, is the reinforced at trial t minus the value of the previous trial
      delta1[count1]<-(r[t]-Q1[count1])
      # then, on each trial, the value is updated as Qt+1(ct) = Qt(ct) + α · δ
      Q1[count1+1]<- Q1[count1]+ alpha* delta1[count1]
      PE[t]<-(delta1[count1])
      count1<-count1+1
    } else if (fileSub$object_cat[t]==2){
      delta2[count2]<-(r[t]-Q2[count2])
      Q2[count2+1]<- Q2[count2]+ alpha* delta2[count2]
      PE[t]<-(delta2[count2])
      count2<-count2+1
    } else {
      delta3[count3]<-(r[t]-Q3[count3])
      Q3[count3+1]<- Q3[count3]+ alpha* delta3[count3]
      PE[t]<-(delta3[count3])
      count3<-count3+1
    }
    # this was the learning model, now we need an observation model. In reinforcement learning
    # it is assumed that subjects choose probabilistically according to a softmax distribution
    choice1[t]<-exp(beta *tail(Q1,1))/(exp(beta *tail(Q1,1))+exp(beta*tail(Q2,1))+exp(beta *tail(Q3,1))) #the tail functions determines the last value of the array
    choice2[t]<-exp(beta *tail(Q2,1))/(exp(beta *tail(Q1,1))+exp(beta*tail(Q2,1))+exp(beta *tail(Q3,1)))
    choice3[t]<-exp(beta *tail(Q3,1))/(exp(beta *tail(Q1,1))+exp(beta*tail(Q2,1))+exp(beta *tail(Q3,1)))
    # take the sum oof the log of the choice to avoid minimum floating point problem, as suggested by Daw(2011)
    choice1log[t]<-beta *tail(Q1,1)-log((exp(beta *tail(Q1,1))+exp(beta*tail(Q2,1))+exp(beta *tail(Q3,1)))) #the tail functions determines the last value of the array
    choice2log[t]<-beta *tail(Q2,1)-log((exp(beta *tail(Q1,1))+exp(beta*tail(Q2,1))+exp(beta *tail(Q3,1)))) #the tail functions determines the last value of the array
    choice3log[t]<-beta *tail(Q3,1)-log((exp(beta *tail(Q1,1))+exp(beta*tail(Q2,1))+exp(beta *tail(Q3,1)))) #the tail functions determines the last value of the array
  }
  # regurn the probability associated with every category, plus the likelihood as the sum of the log all the choice log (see Daw (2022))
  output<-list(Q1, Q2, Q3, choice1, choice2, choice3, prod(choice1), prod(choice2), prod(choice3), PE, abs(PE) )
  return(output)
  
}


####################################################################################################################################
# create a function to find the alpha and beta parameters that minimize the log likelihood
minlogLik<- function(category){
  Alpha<-seq(0.01, 1, length.out=10 )
  Beta<-seq(10,10, length.out=10)
  likel<-matrix(NA, nrow = 10,ncol = 10)
  for (i in 1:length(Alpha)){ #looping through the alphas
    for (j in 1: length(Beta)){ #looping through the betas
      likel[i,j]<-findlikel(Alpha[i], Beta[j])[[category+6]] # estimating the likelihood for choice c, having those parameters
    }
  }
  # which aare the parameters that minimize the loglikelihood ?
  IND<-which(likel == max(likel), arr.ind=T)
  listPar<-list(Alpha[IND[1]], Beta[IND[2]])
  return(listPar)
}
  

# create the function for the estimated probability trial by trial. 
#it returns 1-probability of choice1 2- probability of choice2 3- probability of choice 3 4- PE 5- unsigned PE
findProb<- function(alpha, beta){
  
  # initialise the reinforcement
  # the reinforcement depends on whether that trial is correct or not
  Qr<-NULL
  for (r in 1 :length(fileSub$scene_cat)){
    if (fileSub$acc[r]==1){
      Qr[r]<-1}else{Qr[r]<-0}
  }
  
  # we initialise the values for each category
  Q1<-0
  Q2<-0
  Q3<-0
  
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
  
  # actual perfromance by participant
  perormance<-NULL
  
  for (t in 1: length(fileSub$scene_cat)){
    
    if (fileSub$object_cat[t]==1){
      # delta, the prediction error, is the reinforced at trial t minus the value of the previous trial
      delta1[count1]<-(Qr[t]-Q1[count1])
      
      # then, on each trial, the value is updated as Qt+1(ct) = Qt(ct) + α · δ
      Q1[count1+1]<- Q1[count1]+ alpha* delta1[count1]
      fileSub$PE[t]<-(delta1[count1])
      count1<-count1+1
      
    } else if (fileSub$object_cat[t]==2){
      delta2[count2]<-(Qr[t]-Q2[count2])
      Q2[count2+1]<- Q2[count2]+ alpha* delta2[count2]
      fileSub$PE[t]<-(delta2[count2])
      count2<-count2+1
    } else if (fileSub$object_cat[t]==3){
      delta3[count3]<-(Qr[t]-Q3[count3])
      Q3[count3+1]<- Q3[count3]+ alpha* delta3[count3]
      fileSub$PE[t]<-(delta3[count3])
      count3<-count3+1
    }
    
    # this was the learning model, now we need an observation model. In reinforcement learning
    # it is assumed that subjects choose probabilistically according to a softmax distribution
    choice1[t]<-exp(beta *tail(Q1,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1))) #the tail functions determines the last value of the array
    choice2[t]<-exp(beta *tail(Q2,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))
    choice3[t]<-exp(beta *tail(Q3,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))
  }
  my_list<-list(choice1, choice2, choice3,fileSub$PE, abs(fileSub$PE))
  return(my_list)
}

##################################################################################################################################
# create a funciton to find the estimated parameters, maximising R square
# it returns alpha and beta
findPar<-function(Preference,category){
  # creating a dataframe to put everything in there
  Rsquared<-matrix(NA, nrow = 100,ncol = 10)
  Alpha<-seq(0.01, 1, length.out=100 )
  Beta<-seq(0.1,7, length.out=10)
  for (i in 1:length(Alpha)){ #looping through the alphas
    for (j in 1: length(Beta)){ #looping through the betas
      Prob<-findProb(Alpha[i], Beta[j])[[category]] # estimating the probability of making choice c, having those parameters
      #now calculate the fit as R2. 
      #sum of squared residuals: sum of empirical - thoeretical deviations squared
      SSR<-sum((preference-Prob)^2)
      #sum of squared total: sum of empirical - mean, squared 
      SST<-sum((preference-mean(preference))^2)
      #SSM: sum of square of the model
      SSM<-SST-SSR
      #finally, we get our R square
      Rsquared[i,j]<-SSM/SST
    }
  }
  # which aare the parameters that maximizes the R square?
  IND<-which(Rsquared == max(Rsquared), arr.ind=T)
  listPar<-list(Alpha[IND[1]], Beta[IND[2]])
  return(listPar)
}