###rescorla wagner model for associative learning implementation
#https://psyr.org/programming.html
rm(list=ls())
#formula: deltaV = alpha beta (lambda-sumV)

update_RW <- function(value, alpha, beta, lambda) {
  # compute the value of the compound stimulus Calculate the value (VAB) of the compound stimulus
  #In the Rescorla-Wagner model, the associative strength for the compound is just the sum of the individual strengths, 
  #so I can use the sum function to add up all the elements of the value argument:
  value_compound <- sum(value)
  # compute the prediction error.learning is the prediction error, λU−VAB
  prediction_error <- lambda - value_compound
  # compute the change in strength
  value_change <- alpha * beta * prediction_error 
  # update the association value
  value <- value + value_change
  # return the new value
  value <- value + value_change
  
  
}

#For the first “experiment” to simulate, we’ll pair a simple CS (i.e. not compound) 
#with a US for 20 trials, and examine how the association strength changes over time. 
#So get started, we’ll specify the number of trials
n_trials <- 20  
strength <- numeric(n_trials)

#set alpha at 0.80
alpha<-0.4 #alpha is the learning rate linked to first stimulus
beta<-0.6#beta is the learning rate linked to the category
lambda<-0.80 #lambda is the reward value, which is the maximum possible association strignth for the CS (in our case is 80%)

#create a variable to store the prediction error
prediction_error<-NULL

update_RW <- function(value, alpha=0.4, beta=0.4, lambda=0.80) {
  value_compound <- sum(value)
  prediction_error <- lambda - value_compound
  value_change <- alpha * beta * prediction_error 
  value <- value + value_change
}

#instead of having fixed alphas, 
#create an alpha that follows the sequence of the trials

#the categories trial after trial are the following
objCat<-c(1 ,1 ,1 ,2 ,1 ,1 ,3 ,1, 1, 1, 1, 3, 1, 1 ,1 ,1 ,2 ,1 ,1 ,1)
betaTr<-NULL
for (l in 1: length(objCat)){
  if(objCat[l]==1){
  betaTr[l]<-beta
  }else{betaTr[l]<--beta}
}

#create a variable to store the prediction error
prediction_error<-NULL
for(i in 2:n_trials) {
  value_compound <- sum(strength[i-1])
  prediction_error[i] <- lambda - value_compound
  #here we should change the beta, the learning of the second category
  beta = betaTr[i]
  value_change <- alpha * beta * prediction_error[i] 
  strength[i] <- sum(strength[i-1]) + value_change
}
plot(strength, ylim=c(0,1), type="l", col="red",
     xlab="TrialN", ylab = "%", main = "Phase 1")
lines(prediction_error, type="l")
legend( 1,1,legend=c("Prediction Error", "Preference"),
        cex=0.8, lty = c(1,1), col = c(1,2))
axis(1, at = c(1:length(objCat)),  labels = objCat)

####start new code: add lambda as a difference between matched and un-matched
prediction_error<-NULL
strength <- numeric(n_trials)

#the categories trial after trial are the following
objCat<-c(1 ,1 ,1 ,2 ,1 ,1 ,3 ,1, 1, 1, 1, 3, 1, 1 ,1 ,1 ,2 ,1 ,1 ,1)
lambda<-NULL
value_change<-NULL
for (l in 1: length(objCat)){
  if(objCat[l]==1){
    lambda[l]<-0.80
  }else{lambda[l]<-0.20}
}
for(i in 2:n_trials) {
  value_compound <- sum(strength[i-1])
 # prediction_error[i] <- lambda[i-1] - value_compound
  prediction_error[i] <- lambda[i-1] - value_compound
  #here we should change the beta, the learning of the second category
  #beta = betaTr[i]
  value_change <- alpha  * prediction_error[i] 
  strength[i] <- sum(strength[i-1]) + value_change
}

#####end new code
plot(strength, ylim=c(0,1), type="l", col="red",
     xlab="TrialN", ylab = "%", main = "Phase 1")
lines(prediction_error, type="l")
legend( 1,1,legend=c("Prediction Error", "Preference"),
       cex=0.8, lty = c(1,1), col = c(1,2))
axis(1, at = c(1:length(objCat)),  labels = objCat)

