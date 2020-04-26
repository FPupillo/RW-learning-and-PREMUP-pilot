# new script to create data likelihood, according to Daw (2011)
# "Thu Apr 23 12:56:45 2020"




FINDalphabeta<-function(alpha, beta){
  #beta<-0.4

  # initialise the reinforcement
  # the reinforcement depends on whether that trial is correct or not
  r<-NULL
  for (q in 1 :length(fileSub$scene_cat)){
    if (fileSub$acc[q]==1){
      r[q]<-1}else{r[q]<-0}
  }
  
  # initialize PE
  PE<-NULL
  
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
  
  # we need variables for the implementation of the choice
  choice1<-NULL
  choice2<-NULL
  choice3<-NULL
  
  # we need variables to implement the log likelihood
  choice1log<-NULL
  choice2log<-NULL
  choice3log<-NULL
  

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
      delta3[count3]<-(r[t]-Q2[count3])
      Q3[count3+1]<- Q3[count3]+ alpha* delta3[count3]
      PE[t]<-(delta3[count3])
      count3<-count3+1
    }
    
    
    # this was the learning model, now we need an observation model. In reinforcement learning
    # it is assumed that subjects choose probabilistically according to a softmax distribution
    
    choice1[t]<-exp(beta *tail(Q1,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1))) #the tail functions determines the last value of the array
    choice2[t]<-exp(beta *tail(Q2,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))
    choice3[t]<-exp(beta *tail(Q3,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))
    # take the sum oof the logof the choice to avoid minimum floating point problem, as suggested by Daw(2011)
    choice1log[t]<-beta *tail(Q1,1)-log((exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))) #the tail functions determines the last value of the array
    choice2log[t]<-beta *tail(Q2,1)-log((exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))) #the tail functions determines the last value of the array
    choice3log[t]<-beta *tail(Q3,1)-log((exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))) #the tail functions determines the last value of the array
    
  }
  # regurn the probability associated with every category, plus the likelihood as the product of all the choice log (see Daw (2022))
  output<-data.frame(Q1, Q2, Q3, choice1, choice2, choice3, sum(choice1log), sum(choice2log), sum(choice3log), PE, abs(PE) )
  return(output)
  
  
}

library(pracma)

beta<-0.5
alpha<-c(0.1,0.2,0.3)


res<-fmincon(x0, FINDalphabeta)
fminsearch(FINDalphabeta, x0=seq(0.1, 1, length.out = 10),beta=seq(0.1, 1, length.out = 10),method="Nelder-Mead")


  plot(likel)
  
  return(sum(choice1log))
}
  

install.packages("pracma")
install.packages("NlcOptim")
library(pracma)
x0<-(0.3,5)
fminsearch(FINDalphabeta,x0)
fmincon(x0, FINDalphabeta)

        