# Rescorla Wagner Model

# creating a function which updates the strength of a stimulus in eliciting CR

# at different values of lambda (uncoditioned stimulus can be either present or not)

update_RW <- function(value, alpha) {
  
  for (n in 1:n_trials){
    
  # values of lambda (similar to the PIVOTAL premup pilot)  
  lambda<-c(1 ,1 ,1 ,0 ,1 ,1 ,0 ,1, 1, 1, 1, 0, 1, 1 ,1 ,1 ,0 ,1 ,1 ,1)
    
  Value <- value[n]    #Value capital letter is a unique value that is taken from the series "value"
  # compute the prediction error.learning is the prediction error, λ−V
  prediction_error[n] <- lambda[n] - Value
  # compute the change in strength
  value_change <- alpha * prediction_error [n]
  # update the association value
  value[n+1] <- Value + value_change
  }
  # return the new values
  variables<-data.frame(value[1:n_trials], prediction_error, 1:n_trials)
  names(variables)<-c("value", "Prediction_error", "trial")
  return(variables)
}

# use this function

# initialise variables
n_trials <- 20            # assuming 20 trials
value <- numeric(n_trials)# initialising value at 0
prediction_error<-NULL

alpha <- 0.3 # we are setting alpha at 0.3, which is consuetudinal

RWM<-update_RW(value, alpha)


# plot it
plot(RWM$value, ylim = c(-1, 1),type="l", col="black", ylab="", xlab="Trial", xaxt="n")
abline(h=c(-1.0,-0.2,-0.4,-0.6,-0.80,0.2,0.4,0.6,0.8,1.0), lty=1, col="grey")#for unsined PE
abline(h=0, lty=1, col="black")#for the zero
lines(RWM$Prediction_error, col="red")
abline(v=seq(1:20), lty=1, col="grey")
axis(1, at = 1:20)

#add the values of lambda
lambda<-c(1 ,1 ,1 ,0 ,1 ,1 ,0 ,1, 1, 1, 1, 0, 1, 1 ,1 ,1 ,0 ,1 ,1 ,1)
axis(3, at = 1:20, labels = lambda)
mtext("Lambda", 3, line=2)

legend("bottomleft", legend = c("value", "prediction error"), lty = c(1,1),col  = c("black", "red"))
