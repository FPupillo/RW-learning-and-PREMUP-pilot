# estimating Q-learning model as in Daw (2011)
# [1] "Wed Apr  8 13:15:53 2020"

rm(list=ls())


#we have 1 scene, each scene can be followed by some category with a certain percentage of probability
# retrieve the scene

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



#for (l in 1: 30){

#retrieve the file
file<-read.csv(phase1Files[1])
#subset only trials for scene 1. 
fileSub<-file[file$scene_cat==4,]

# at each trial, participants assign value to each category (1,2,3). 
# we initialise the values for each category
Q1<-0
Q2<-0
Q3<-0

# we initialise alpha, or the learning rate, or the degree to which participants are updating the priors
# it is a number btwen 0 and 1. Let's start with 0.3
Alpha<-seq(0,1,length.out=100)
Beta<-seq(1,10,length.out=10)
#estimate the maximum likelihood
ML<-matrix(NA, nrow = 100,ncol = 10) #three columns: alpha, beta, maximum likelihood
# then the beta, the inverse temperature
#beta<-6

# initialise the reinforcement
# the reinforcement depends on whether that trial is correct or not
Qr<-NULL
for (r in 1 :length(fileSub$scene_cat)){
  if (fileSub$acc[r]==1){
    Qr[r]<-1}else{Qr[r]<-0}
}

# apply the learning rate formula by updating it at every trial
for (i in 1: 100){
  for (j in 1:10){
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

# take the log of the choice
choice1log<-NULL




  alpha<-Alpha[i]
  beta<-Beta[j]
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

#choice[t]<-which(c(choice1[t], choice2[t], choice3[t])==max(choice1[t], choice2[t], choice3[t]))


#choice[t]<-sample(c(1,2,3),1,prob=c(choice1[t], choice2[t], choice3[t]))

# rand<-NULL
#   for (i in 1:10){
#   rand[i]<-sample(c(1,2,3),1,prob=c(choice1[t], choice2[t], choice3[t]))
#   }
# choice[t]<-median(rand)
# 
# performanceTh[t]<-sum(choice==1)/t

# retrieve actual performance

ACTchoice<-NULL# actual choice made by participants
performance<-NULL   #actual performance by participant, as the sum of the expected choice over the sum of the trials
for (e in 1 :length(fileSub$scene_cat)){
  if(fileSub$response[e]==1){ACTchoice[e]<-1
  }else{ACTchoice[e]<-0}
  performance[e]<-sum(ACTchoice)/e
} 

}
  # maximum likelihood
  sum(choice1log)
  choice<-prod(choice1)
  ML[i,j]<- (choice)
  }
  print(i)
}
# plot(choice1, ylim=c(0,1), xlim=c(1, 20),xaxt="n",xlab="",ylab="Preference for Category")
# title(main = paste("alpha = ", alpha, ",","beta = ",beta), line=3)
# abline(h=c(0.,0.2,0.4,0.6,0.8), lty=1, col="grey")#
# abline(v=seq(1:20), lty=1, col="grey")#
# axis(3, at = c(1:length(fileSub$scene_cat)),  labels = fileSub$object_cat)
# mtext("Obj cat", 3, line=1,at =-1)
# axis(1, at = c(1:length(fileSub$scene_cat)),  labels = fileSub$acc)
# mtext("Accuracy", 1, line=2)
# 
# lines(fileSub$PE)
# # lines(performanceTh, col="red")
# lines(performance, col="blue")
heatmap(ML, Colv = NA, Rowv = NA, scale="column")

