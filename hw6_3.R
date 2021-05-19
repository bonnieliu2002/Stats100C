a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/jura.txt", header=TRUE)
n <- nrow(a)
y <- a$Pb
X <- cbind(rep(1, n), a$Cd, a$Co, a$Cr, a$Cu, a$Ni, a$Zn)

# Part A: Estimate Beta2 using partial regression

# X1 has first 4 columns
# X2 has last 2 columns
X1 <- X[, 1:4]
X2 <- X[, 5:7]

# 1) Regress y on X1 and take the residuals (remove the contributions of variables of X1 on y)
Hx1 <- X1 %*% solve(t(X1) %*% X1) %*% t(X1)
yStar <- (diag(n) - Hx1) %*% y

# 2) Regress X2 on X1 and take the residuals (remove the shared characteristics of X1 and X2)
X2Star <- (diag(n) - Hx1) %*% X2

# 3) Regress yStar on X2Star and take the estimated coefficients
Beta2Hat = solve(t(X2Star) %*% X2Star) %*% t(X2Star) %*% yStar

# To double check our estimate of Beta2Hat, we can run lm() and compare the estimates.
# We see that our estimates match.
modl <- lm(y ~ X + 0)
summary(modl)

# Part B: Estimate the slopes by regressing the deviations of the response variable on the deviations of the predictors

X1 <- X[, 1]
X2 <- X[, 2:7]

# Construct the "mean sweeper" matrix (I - 1/n 11') to help with calculations
meanSweeper = diag(n) - (1/n) * rep(1, n) %*% t(rep(1, n))

# 1) Regress y on X1 to obtain residuals
yStar <- meanSweeper %*% y

# 2) Regress X2 on X1 to obtain residuals
X2Star <- meanSweeper %*% X2

# 3) Regress yStar on X2Star to obtain coefficients
estimated_slopes <- solve(t(X2Star) %*% X2Star) %*% t(X2Star) %*% yStar

# We see that our estimates match with the ones obtained using lm()