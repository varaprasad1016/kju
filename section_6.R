###############################################################
# This code covers the code presented in Chapter 6

# Section 6.1 Linear Regression
###############################################################

# Example in R

income_input = as.data.frame(read.csv("C:/Data/income.csv"))
income_input[1:10,]

summary(income_input)

library(lattice)

splom(~income_input[c(2:5)],  groups=NULL, data=income_input, 
      axis.line.tck = 0,
      axis.text.alpha = 0)

results <- lm(Income~Age + Education + Gender, income_input)
summary(results)

results2 <- lm(Income ~ Age + Education, income_input)
summary(results2)






# compute confidence intevals for the model parameters
confint(results2, level = .95)

# compute a confidence interval on the expected income of a person
Age <- 41
Education <- 12
new_pt <- data.frame(Age, Education)

conf_int_pt <- predict(results2, new_pt, level=.95, interval="confidence")
conf_int_pt 

# compute a prediction interval on the income of the same person
pred_int_pt <- predict(results2, new_pt, level=.95, interval="prediction")
pred_int_pt



###############################################################
# Diagnostics 

with(results2, {
  plot(fitted.values, residuals,ylim=c(-40,40) )
  points(c(min(fitted.values),max(fitted.values)), c(0,0), type = "l")})

hist(results2$residuals, main="")

qqnorm(results2$residuals, ylab="Residuals", main="")
qqline(results2$residuals) 







# Section 6.2 Logistic Regression
###############################################################

churn_input = as.data.frame(read.csv("C:/Data/churn.csv"))
head(churn_input)

sum(churn_input$Churned)

Churn_logistic1 <- glm (Churned~Age + Married + Cust_years + Churned_contacts, 
                        data=churn_input, family=binomial(link="logit"))
summary(Churn_logistic1)

Churn_logistic2 <- glm (Churned~Age + Married +  Churned_contacts,
                        data=churn_input, family=binomial(link="logit"))
summary(Churn_logistic2)

Churn_logistic3 <- glm (Churned~Age + Churned_contacts, 
                        data=churn_input, family=binomial(link="logit"))
summary(Churn_logistic3)




# Receiver Operating Characteristic (ROC) Curve

#install.packages("ROCR")     #install, if necessary
library(ROCR)

pred = predict(Churn_logistic3, type="response")
predObj = prediction(pred, churn_input$Churned )

rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc")  

plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4))) 


