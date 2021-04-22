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
x <- seq(-10, 10, 0.05)
y <- dt(x, n-2, ncp=0)
plot(x, y, type="l", lwd=5, ylab=substitute(f(x)), xlim=c(-11,11))

# noncentral t-distribution
beta1 <- 0.05
sigma2 <- 600
ncp1 <- beta1 * sqrt((n - 1)*var(a$zinc)) / sqrt(sigma2)
x <- seq(-10, 10, 0.05)
y <- dt(x, n-2, ncp=ncp1)
points(x, y, type="l", lwd=5, col="blue", ylab=substitute(f(x)))

# compute critical value (rejection region using alpha=0.05)
crit_val_upper = qt(.975, n-2)
print(crit_val_upper)
crit_val_lower = qt(.025,  n-2)
print(crit_val_lower)
# compute the power when beta1=0.05 and sigma2=600
print(pt(crit_val_upper, n-2, ncp=ncp1, lower.tail=FALSE) + pt(crit_val_lower, n-2, ncp=ncp1))