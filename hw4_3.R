# read in the table
a1 <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/soil.txt", header=TRUE)

# n is the number of observations
n = 30
# only want to use first 30 rows of data set
a <- a1[1:n, ]

# apply linear model with lead as our response variable and zinc as predictor variable
q <- lm(a$lead ~ a$zinc)
summary(q)

# central t-distribution
x <- seq(-10, 10, 0.5)
y <- dt(x, n-2, ncp=0)
plot(x, y, type="l", lwd=5, ylab=substitute(f(x)), xlim=c(-11,11))

# # noncentral t-distribution
# beta1 <- 0.05
# sigma2 <- 600
# ncp <- (beta1-0)*sqrt()
# 
# print(var(a$zinc))

# # b1 is Beta_1_hat
# b1 <- q$coef[2]
# # Se is the square root of sample variance
# Se <- summary(q)$sigma
# 
# # t-statistic
# t <- b1 / (Se/sqrt(sum((a$zinc - mean(a$zinc))^2))) # calculated from equation from 3a
# print(t) # prints 18.22474
# print(2 * (1 - pt(t, n-1))) # two-side t-test; prints 0