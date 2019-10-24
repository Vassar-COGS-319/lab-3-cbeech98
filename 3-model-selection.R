# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# parameter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark.

## RANDOM WALK
rand.test <- random.walk.model(1000, drift=0.012, sdrw=0.3, criterion=4.9)

#Accuracy
sum(rand.test$correct) / length(rand.test$correct) # should be close to 0.80

rand.correct.rt <- rand.test %>% filter(correct==TRUE) %>% select(rt) %>% unlist()
mean(rand.correct.rt) # should be 250 ms

rand.incorrect.rt <- rand.test %>% filter(correct==FALSE) %>% select(rt) %>% unlist()
mean(rand.incorrect.rt) # should be 246 ms

## ACCUMULATOR
acc.test <- accumulator.model(1000, rate.1=85, rate.2=92, criterion=3)

#Accuracy
sum(acc.test$correct) / length(acc.test$correct) # should be close to 0.80

acc.correct.rt <- acc.test %>% filter(correct==TRUE) %>% select(rt) %>% unlist()
mean(acc.correct.rt) # should be 250 ms

acc.incorrect.rt <- acc.test %>% filter(correct==FALSE) %>% select(rt) %>% unlist()
mean(acc.incorrect.rt) # should be 246 ms

# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:

# Yes, both models can do a reasonable job of accounting for the mean RTs and accuracy.
# random walk: accuracy = 0.793, rt correct = 244.4565, rt incorrect = 258.3043
# accumulator: accuracy = 0.819, rt correct = 252.3065, rt incorrect = 259.5801

# We couldn't reliably get rt incorrect to be less than rt correct, although it does happen occasionally.

# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.

par(mfrow=c(2,2))
hist(rand.correct.rt)
hist(rand.incorrect.rt)
hist(acc.correct.rt)
hist(acc.incorrect.rt)

par(mfrow=c(1,1))

# Based on these distributions, we could use the range of predicted reaction 
# times to evaluate which model is a better descriptor of the data for the 
# experiment. For the accumulator model, the range is much smaller (~200 to 
# 300). If the range of reaction times observed in the data is also small, we 
# would likely prefer this model.

# I amended my response to this last question because I realized that the model
# with less variable predicted reaction times might not always be desirable.
