rm(list = ls())

library(ggplot2)
library(aod)
library(Rcpp)

dat0 <- data.frame(read.csv("http://www.ats.ucla.edu/stat/data/binary.csv"))
head(dat0)
summary(dat0)

sapply(dat0, sd)

#table(dat0$admit,dat0$rank)
#For categorical data, looking at contingency table
xtabs(~ admit + rank, data = dat0)

#Using the logit model
dat0$rank <- as.factor(dat0$rank)

logit <- glm(admit ~ gre + gpa + rank, data = dat0, family = "binomial")
summary(logit)

#Obtaining confidence intervals using profiled log-liklihood
confint(logit)

#Obtaining confidence intervals using standard errors
confint.default(logit)

#Can test the overall effect of rank
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 4:6)

#Interpreting the odds ratios
exp(coef(logit))

#Interpreting the odds ratio and 95% CI
exp(cbind(OR = coef(logit), confint(logit)))

#Calculating the predicted probability of admission at each 
#value of rank, holding gre and gpa at their means.

dat1 <- with(dat0, data.frame(gre = mean(gre), gpa = mean(gpa),
             rank = factor(1:4)))
dat1

#Prediction

dat1$rankprob <- predict(logit, newdata = dat1, type = "response")
dat1

#Preparing data to plot 

dat2 <- with(dat0, 
             data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4),
             gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))


dat3 <- cbind(dat2, predict(logit, newdata = dat2, type = "link", se = TRUE))
dat3 <- within(dat3, {
               PredictedProb <- plogis(fit)
               LL <- plogis(fit - (1.96*se.fit))
               UL <- plogis(fit + (1.96*se.fit))
})

head(dat3)

#Plot

ggplot(dat3, aes(x = gre, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) +
  geom_line(aes(colour = rank), size = 1)

#Testing Model Fit

with(logit, null.deviance - deviance)
with(logit, df.null - df.residual)

#Evaluating p-value
with(logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Log Liklihood
logLik(logit)




#Reference:R Data Analysis Examples: Logit Regression
#from http://www.ats.ucla.edu/stat/r/dae/logit.htm