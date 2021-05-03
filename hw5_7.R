a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/jura.txt", header=TRUE)
n <- nrow(a)
y <- a$Pb
X <- cbind(rep(1,n), a$Cd, a$Cu, a$Zn)

# Part 1
# find X'X
XtX <- t(X) %*% X
# find (X'X)^(-1)
XtXinv <- solve(XtX)
# find X'Y
XtY <- t(X) %*% y

# Part 2
# find beta_hat
beta_hat = XtXinv %*% XtY

# Part 3
# find H
H = X %*% XtXinv %*% t(X)

# Part 4
# find y_hat
y_hat = X %*% beta_hat

# Part 5
# find e
e = y - y_hat