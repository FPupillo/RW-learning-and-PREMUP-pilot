#### Learning rule from Daw (2011)

updateRL<-function(alpha, beta){
  
# inizialise the reinforcement, which is the feedback. 1 meaning correct and 0 meaning incorrect
r<- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1) 

# we also need a variable for the object category
objCat<-c(1 ,1 ,1 ,2 ,1 ,1 ,3 ,1, 1, 1, 1, 3, 1, 1 ,1 ,1 ,2 ,1 ,1 ,1)
# this is assumed accuracy for a participant. 

# at each trial, participants assign value to each category (1,2,3). 
# we initialise the values for each category at 0
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
# finally, we need variables for the implementation of the choice
choice1<-NULL
choice2<-NULL
choice3<-NULL
# initialise PE
PE<-NULL

# loop through the trials
for (t in 1 :20){
  if (objCat[t]==1){
    # delta, the prediction error, is the reinforced at trial t minus the value of the previous trial
    delta1[count1]<-(r[t]-Q1[count1])
    PE[t]<-delta1[count1]
    # then, on each trial, the value is updated as Qt+1(ct) = Qt(ct) + α · δ
    Q1[count1+1]<- Q1[count1]+ alpha* delta1[count1]
    count1<-count1+1
  } else if (objCat[t]==2){
    delta2[count2]<-(r[t]-Q2[count2])
    PE[t]<-delta2[count2]
    Q2[count2+1]<- Q2[count2]+ alpha* delta2[count2]
    count2<-count2+1
  } else {
    delta3[count3]<-(r[t]-Q2[count3])
    PE[t]<-delta3[count3]
    Q3[count3+1]<- Q3[count3]+ alpha* delta3[count3]
    count3<-count3+1
  }
  # this was the learning model, now we need an observation model. In reinforcement learning
  # it is assumed that subjects choose probabilistically according to a softmax distribution
  choice1[t]<-exp(beta *tail(Q1,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1))) #the tail functions determines the last value of the array
  choice2[t]<-exp(beta *tail(Q2,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))
  choice3[t]<-exp(beta *tail(Q3,1))/(exp(beta *tail(Q1,1))+exp(tail(Q2,1))+exp(beta *tail(Q3,1)))
}
output<-data.frame(choice1, choice2, choice3,PE)
names(output)<-c("choce1", "choice2", "choice3", "PE")
return(output)
}

# RLmodel<-updateRL(0.15,2.4)

# now plot it
# plot it
# plot(RLmodel$choce1, ylim = c(-1, 1),type="l", col="black", ylab="", xlab="Trial", xaxt="n", yaxt="n")
# axis(2, at=c(-1.0,-0.2,-0.4,-0.6,-0.80,0, 0.2,0.4,0.6,0.8,1.0))
# abline(h=c(-1.0,-0.2,-0.4,-0.6,-0.80,0.2,0.4,0.6,0.8,1.0), lty=1, col="grey")#for unsined PE
# abline(h=0, lty=1, col="black")#for the zero
# lines(RLmodel$PE, col="red")
# abline(v=seq(1:20), lty=1, col="grey")
# axis(1, at = 1:20)
# axis(3, at = c(1:20),  labels = r)
# mtext("Accuracy", 3, line=2)
# legend("bottomleft",inset =0.1, legend = c(expression(paste(P['j=M'], ", (c=I)")), "prediction error"), lty = c(1,1),col  = c("black", "red"))


