effluent <- read.table("C:\\Users\\Beans\\Desktop\\BRIDGE MAN\\Junior Year\\Spring Semester\\Applied Multivariate Analysis\\Data\\T6-1.dat", header = F, col.names = c("x1b", "x2b", "x1a", "x2a"))
attach(effluent)
effluent <- log(effluent)
D <- cbind(x1b - x1a, x2b - x2a)
Xbar <- colMeans(D)
S <- var(D)
T2 <- 11 * t(Xbar) %*% solve(S) %*% Xbar
T2
crit <-10 * 2 / 9 * qf(0.05, 2, 9, lower.tail = F)
crit
treatment2 <- matrix(c(3, 3, 1, 6, 2, 3), nrow = 3, byrow = T)
treatment3 <- matrix(c(2, 3, 5, 1, 3, 1, 2, 3), nrow = 4, byrow = T)
t2xbar <- colMeans(treatment2)
t3xbar <- colMeans(treatment3)
t2S <- var(treatment2)
t3S <- var(treatment3)
n1 <- 3
n2 <- 4
Spool <- (n1 - 1) / (n1 + n2 - 2) * t2S + (n2 - 1) / (n1 + n2 - 2) * t3S
Spool
T2 <- t(t2xbar - t3xbar) %*% solve((1 / n1 + 1 / n2) * Spool) %*% (t2xbar - t3xbar)
T2
crit <- (n1 + n2 - 2) * 2 / (n1 + n2 - 2 - 1) * qf(0.99, 2, n1 + n2 - 2 - 1)
crit
X11 <- c(6, 5, 8, 4, 7)
X21 <- c(3, 1, 2)
X31 <- c(2, 5, 3, 2)
X11bar <- mean(X11)
X11bar
X21bar <- mean(X21)
X21bar
X31bar <- mean(X31)
X31bar
X1 <- c(6, 5, 8, 4, 7, 3, 1, 2, 2, 5, 3, 2)
X1bar <- mean(X1)
X1bar
X12 <- c(7, 9, 6, 9, 9)
X22 <- c(3, 6, 3)
X32 <- c(3, 1, 1, 3)
X12bar <- mean(X12)
X12bar
X22bar <- mean(X22)
X22bar
X32bar <- mean(X32)
X32bar
X2 <- c(7, 9, 6, 9, 9, 3, 6, 3, 3, 1, 1, 3)
X2bar <- mean(X2)
X2bar
B <- matrix(c(36, 48, 48, 84), nrow = 2, byrow = T)
W <- matrix(c(18, -13, -13, 18), nrow = 2, byrow = T)
Wilks <- det(W)/det(B + W)
Wilks
value <- 4 * ((1 - sqrt(Wilks))/(sqrt(Wilks)))
value
crit <- qf(0.99, 4, 16)
crit
bartlett <- (-15.5) * log(Wilks)
bartlett
crit2 <- qchisq(0.99, 4)
crit2
