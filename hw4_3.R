# read in the table
a1 <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/soil.txt", header=TRUE)
# n is the number of observations
n = 30
# only want to use first 30 rows of data set
a <- a1[1:n, ]
# apply linear model with lead as our response variable and zinc as predictor variable
q <- lm(a$lead ~ a$zinc)

# central t-distribution (black curve)
x <- seq(-7, 10, 0.05)
y <- dt(x, n-2, ncp=0)
plot(x, y, type="l", lwd=5, ylab=substitute(f(x)), xlim=c(-8,11))

# noncentral t-distribution (blue curve)
beta1 <- 0.05
pop_var <- 600
our_ncp <- beta1 * sqrt((n - 1)*var(a$zinc)) / sqrt(pop_var) # using equation from 3c
x <- seq(-7, 10, 0.05)
y <- dt(x, n-2, ncp=our_ncp)
points(x, y, type="l", lwd=5, col="blue", ylab=substitute(f(x)))

# compute critical value (rejection region using alpha=0.05)
crit_val_upper = qt(.975, n-2)
print(crit_val_upper) # prints 2.048407
abline(v=crit_val_upper, col="red", lwd=2.5)
crit_val_lower = qt(.025,  n-2)
print(crit_val_lower) # prints -2.048407
abline(v=crit_val_lower, col="red", lwd=2.5)
# compute the power when beta1=0.05 and sigma2=600
print(pt(crit_val_upper, n-2, ncp=our_ncp, lower.tail=FALSE) + pt(crit_val_lower, n-2, ncp=our_ncp)) # prints 0.9208974