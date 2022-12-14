
##########################################
# section 3.3 Statistical Methods for Evaluation
##########################################


##########################################
# section 3.3.1 Hypothesis Testing
##########################################

x <- 1:14
x

# generate random observations from the two populations
x <- rnorm(100, mean=100, sd=5) # normal distribution centered at 100
y <- rnorm(200, mean=105, sd=5) # normal distribution centered at 105

# Student's t-test
t.test(x, y, var.equal=TRUE) # run the Student's t-test

# obtain t value for a two-sided test at a 0.05 significance level
qt(p=0.05/2, df=28, lower.tail= FALSE)

# Welch's t-test
t.test(x, y, var.equal=FALSE) # run the Welch's t-test

# Wilcoxon Rank-Sum Test
wilcox.test(x, y, conf.int = TRUE)




x=c(5.5, 6.9,  5.2,   7.4,  9.7,  7.8,  6.2,  8.4, 8.8)
y=c(7.9, 10.7, 12.4, 6.5, 13.8, 7.2, 15.9, 8.5) 

# Student's t-test
t.test(x, y, var.equal=TRUE) # run the Student's t-test

# Welch's t-test
t.test(x, y, var.equal=FALSE) # run the Welch's t-test






##########################################
# section 3.3.6 ANOVA
##########################################

offers <- sample(c("offer1", "offer2", "nopromo"), size=500, replace=T)

# Simulated 500 observations of purchase sizes on the 3 offer options
purchasesize <- ifelse(offers=="offer1", rnorm(500, mean=80, sd=30),
                       ifelse(offers=="offer2", rnorm(500, mean=85, sd=30),
                              rnorm(500, mean=40, sd=30)))

# create a data frame of offer option and purchase size
offertest <- data.frame(offer=as.factor(offers),
                        purchase_amt=purchasesize)

# display a summary of offertest where offer="offer1"
summary(offertest[offertest$offer=="offer1",])

# display a summary of offertest where offer="offer2"
summary(offertest[offertest$offer=="offer2",])

# display a summary of offertest where offer="nopromo"
summary(offertest[offertest$offer=="nopromo",])

# fit ANOVA test
model <- aov(purchase_amt ~ offers, data=offertest)
summary(model)


