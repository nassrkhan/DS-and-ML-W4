plot(rundata$FirstRun, rundata$SecondRun)

plot(rundata$FirstRun, rundata$SecondRun)
reg1 <- lm( rundata$SecondRun ~rundata$FirstRun)

abline(reg1)

cor(rundata$FirstRun, rundata$SecondRun)

#Check if the Correlation is significant
cor.test(rundata$FirstRun, rundata$SecondRun)

#Create a Multiple Regression Model to 
#predict the distance achieved in the final run 
#based on the previous runs

lma <- lm(rundata$FinalRun ~ FirstRun + SecondRun, data=rundata)

summary(lma)

help("~")

#Predict the final run distance based on a new 
#runner who has only ran the first 2 runs

lma <- lm(FinalRun ~ FirstRun + SecondRun, data=rundata)
newdata <- data.frame(FirstRun=22, SecondRun=25)
predict(lma, newdata)

#Simple Regression Tutorial

set.seed(12345)
run2 <- rundata[ order( runif(99) ) , ]
lma <- lm(FinalRun ~ FirstRun + SecondRun, data=run2[c(1:80),])
testing <- run2[c(81:99), ]
predictions <- predict(lma, testing)

diff <- predictions - testing$FinalRun
diff_ABS <- abs(diff)
mean(diff)
hist(diff_ABS)
boxplot(diff_ABS)

length( diff[diff> 0] )

length( diff_ABS[diff_ABS<=2] )

testing$prediction <- predictions
testing$difference <- diff_ABS

#Now build a logistic regression model
rundata$superrun = ifelse(rundata$FinalRun >= 45, 1, 0)
logitModel <- glm(superrun ~ FirstRun + SecondRun, data=rundata, family=binomial(link="logit"))
summary(logitModel)

#Compute the Odds Ratios and Confidence Intervals
exp(coef(logitModel))
exp(cbind(OR = coef(logitModel), confint(logitModel)))
