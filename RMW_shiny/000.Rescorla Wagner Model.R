# Rescorla Wagner Model

# creating a function which updates the strength of a stimulus in eliciting CR

update_RW <- function(value, alpha, lambda) {
  
  for (n in 1:n_trials){
    
  Value <- value[n]    #Value capital letter is a unique value that is taken from the series "value"
  # compute the prediction error.learning is the prediction error, λ−V
  prediction_error[n] <- lambda - Value
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
lambda<-1 # we are setting alpha at 1, meaning that the CS is always followed by US

RWM<-update_RW(value, alpha, lambda)

# plot it
plot(RWM$value, type="l", col="black", ylab="", xlab="Trial", xaxt="n")
lines(RWM$Prediction_error, col="red")
axis(1, at = 1:20)
legend(x=9, y=0.5, legend = c("value", "prediction error"), lty = c(1,1),col  = c("black", "red"))
