# Homework 5 Exercise 5

# multiple regression model: Y = X * Beta + Epsilon
# goal: Show that the sum of the elements of any column of the hat matrix H is equal to 1.
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics100C/jura.txt", header=TRUE)
n <- nrow(a)
y <- a$Pb
X <- cbind(rep(1,n), a$Cd, a$Cu, a$Zn) # includes intercept

XtX <- t(X) %*% X
XtXinv <- solve(XtX)
H = X %*% XtXinv %*% t(X)

print(colSums(H)) # all ones



# Now if we remove the intercept, this is no longer the case.
X1 <- cbind(a$Cd, a$Cu, a$Zn) # no intercept

X1tX1 <- t(X1) %*% X1
X1tX1inv <- solve(X1tX1)
H1 = X1 %*% X1tX1inv %*% t(X1)

print(colSums(H1)) # NOT all ones anymore