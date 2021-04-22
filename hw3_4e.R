# read in the table
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/soil.txt", header=TRUE)
# apply linear model with lead as our response variable and zinc as predictor variable
q <- lm(a$lead ~ a$zinc)

# n is the number of observations
n <- nrow(a)
# b1 is Beta_1_hat
b1 <- q$coef[2]
# Se is the square root of sample variance
Se <- summary(q)$sigma

# t-statistic
t <- (b1-0.4) / (Se/sqrt(sum((a$zinc)^2))) # calculated from equation from 4a
print(t) # prints -24.63953
print(2 * pt(t, n-1)) # two-side t-test; prints 2.652496e-55

# F-statistic
f <- (b1-0.4)^2 * sum((a$zinc)^2) / Se^2 # calculated from equation from 4b
print(f) # prints 607.1066
print(pf(f, 1, n-1)) # prints 1