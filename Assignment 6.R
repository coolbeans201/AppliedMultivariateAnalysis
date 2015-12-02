sweat <- read.table("C:\\Users\\Beans\\Desktop\\BRIDGE MAN\\Junior Year\\Spring Semester\\Applied Multivariate Analysis\\Data\\T5-1.dat", header = F, col.names=c("x1", "x2", "x3"))
attach(sweat)
sweat <- as.matrix(sweat)
S <- var(sweat)
eigen(S)
crit <- sqrt(3*19/(20 * 17) * qf(.1,3, 17, lower.tail = F))  
crit
police <- read.table("C:\\T5-8.DAT", header = F, col.names=c("x1","x2","x3","x4","x5"))
attach(police)
police <- as.matrix(police)
x3 <- police[, 3]
m <- mean(police[, 3])
s <- sd(x3)
UCL <- m + 3*s
LCL <- m - 3*s
plot(seq(1:16), x3,'b',ylim = c(-1000, 7000))
lines(seq(1:16), rep(UCL,16))
lines(seq(1:16), rep(0,16), lty = 2)
lines(seq(1:16), rep(LCL,16))
lines(seq(1:16), rep(m,16))
x4 <- police[, 4]
m <- mean(police[, 4])
s <- sd(x4)
UCL <- m + 3*s
LCL <- m - 3*s
plot(seq(1:16), x4,'b',ylim = c(9000, 18000))
lines(seq(1:16), rep(UCL,16))
lines(seq(1:16), rep(LCL,16))
lines(seq(1:16), rep(m,16))
Xbar <- colMeans(cbind(x3, x4)) 
Xbar
cor(x3,x4)
sd(x3)
sd(x4)
library(ellipse)
plot(ellipse(cor(x3, x4), scale = c(sd(x3), sd(x4)), centre = c(2676.875, 13563.625), level = 0.95), type = 'l', ylim = c(-1000, 17000))
points(x3, x4, pch = 11)
identify(police)
X <- cbind(x3,x4) 
S <- var(X) 
dist2 <- c() 
for (i in 1:16){ 
d <- X[i, ] - Xbar 
dist2[i] <- t(d) %*% solve(S) %*% d 
} 
qchisq(0.95, df = 2)
plot(seq(1:16), dist2, xlab = 'period', ylab = 'T^2', pch = 11, ylim = c(0, 6))
lines(seq(1:16), rep(qchisq(.95, df = 2),16), lty = 2)
