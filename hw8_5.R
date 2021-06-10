a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/jura.txt", header=TRUE)
n <- nrow(a)
k <- 6

y <- a$Pb
x1 <- a$Cd
x2 <- a$Co
x3 <- a$Cr
x4 <- a$Cu
x5 <- a$Ni
x6 <- a$Zn

# Part A: construct X
X <- cbind(rep(1, n), x1, x2, x3, x4, x5, x6)

# Part B: compute Beta_hat, unbiased estimator of variance, and hat matrix
Beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
H <- X %*% solve(t(X) %*% X) %*% t(X)
Se2 <- (t(y) %*% (diag(n) - H) %*% y) / (n - k - 1)

# Part C: test overall significance of model using F test for general linear hypothesis
C <- cbind(rep(0, k), diag(k))
gamma <- rep(0, k)
F <- t(C %*% Beta_hat - gamma) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% (C %*% Beta_hat - gamma) / (k * Se2)

# Part D: test H0: (beta1, beta3)' = 0
C <- matrix(c(0, 1, 0, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 0, 0), nrow=2, byrow=TRUE)
m <- 2
gamma <- rep(0, m)
F <- t(C %*% Beta_hat - gamma) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% (C %*% Beta_hat - gamma) / (m * Se2)
pf(F, m, n - k - 1)

# Part E: use the extra sum of squares principle for Part D
df_f = n - k - 1
SSE_f = Se2 * df_f
reduced_model <- lm(Pb ~ Co + Cu + Ni + Zn, data=a)
summ <- summary(reduced_model)
df_r = n - 4 - 1
SSE_r <- (summ$sigma)^2 * df_r
F = ((SSE_r - SSE_f) / (df_r - df_f)) / (SSE_f / df_f)
pf(F, m, n - k - 1)