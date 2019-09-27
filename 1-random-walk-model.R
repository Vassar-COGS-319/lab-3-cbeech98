# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# drift is the drift rate (default value is 0)
# sdrw is the variability in the drift rate (default value is 0.3)
# criterion is the threshold for a response (default value is 3)

library(dplyr)

random.walk.model <- function(samples, drift=0, sdrw=0.3, criterion=3){
  # On each trial, start the internal evidence signal at 0.
  # Sample a value from a normal distribution with mean drift and standard deviation sdrw.
  # Add this value to the internal evidence signal.
  # Repeat steps 2 & 3 until the evidence signal is greater than criterion or less than -criterion.
  # Report the number of samples it took to reach the threshold.
  # If the model reaches criterion then it has responded correctly. 
  # If it reaches -criterion it has responded incorrectly. The number of samples it takes to reach the threshold is the number of milliseconds it took to respond. (So the model assumes that one sample is taken every millisecond.)
  # Steps 1-5 can be repeated many times to generate a distribution of responses.

  accuracy.array <- vector()
  rt.array <- vector()
  
  for(i in 1:samples) {
    es <- 0
    counter <- 0
    while(abs(es) <= criterion) {
      es <- es + rnorm(1, mean=drift, sd=sdrw)
      counter <- counter + 1
    }
    accuracy.array[i] <- if_else(es>0, TRUE, FALSE)
    rt.array[i] <- counter
  }
  
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}

# test the model ####

# if the model is working correctly, then the line below should generate a data frame with 
# 1000 samples and about half of the samples should be correct. the average rt will probably
# be around 112, but might vary from that by a bit.

initial.test <- random.walk.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

# we can use dplyr to filter the data and visualize the correct and incorrect RT distributions

correct.data <- initial.test %>% filter(correct==TRUE)
incorrect.data <- initial.test %>% filter(correct==FALSE)

hist(correct.data$rt)
hist(incorrect.data$rt)
