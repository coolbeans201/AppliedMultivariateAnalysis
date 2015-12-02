sales <- read.table("C:\\Users\\Beans\\Desktop\\BRIDGE MAN\\Junior Year\\Spring Semester\\Applied Multivariate Analysis\\Data\\T9-12.DAT", header = F, col.names = c("x1", "x2", "x3", "x4", "x5", "x6", "x7"))
attach(sales)
sales <- as.matrix(sales)
n <- 50 
sdat <- (sales - matrix(rep(1, n), nrow = n) %*% colMeans(sales)) %*% diag(1 / sqrt(diag(cov(sales)))) 
mle1 <- factanal(sdat, 2) # Perform factor analysis using MLE # and 2 common factors. Varimax rotation is default.
mle1
Xbar1 <- matrix(c(-1, -1), nrow = 2) 
Xbar2 <- matrix(c(2, 1), nrow = 2)
Spool <- matrix(c(7.3, -1.1, -1.1, 4.8), nrow = 2, byrow = T) 
n1 <- 11
n2 <- 12
T2 <- t(Xbar1 - Xbar2) %*% solve((1 / n1 + 1 / n2) * Spool) %*% (Xbar1 - Xbar2)
T2 # test statistic
crit <- (n1 + n2 - 2) * 2 / (n1 + n2 - 2 - 1) * qf(0.90, 2, n1 + n2 - 2 - 1)
crit # critical value
y0 <- t(Xbar1 - Xbar2) %*% solve(Spool)
m <- .5 %*% t(Xbar1 - Xbar2) %*% solve(Spool) %*% (Xbar1 + Xbar2)
x0 <- matrix(c(0, 1), nrow = 2)
value <- y0 %*% x0
value
iris <- read.table("C:\\Users\\Beans\\Desktop\\BRIDGE MAN\\Junior Year\\Spring Semester\\Applied Multivariate Analysis\\Data\\T11-5.DAT", header = F, col.names = c("x1", "x2", "x3", "x4", "sample"))
attach(iris)
plot(x2[sample == 1], x4[sample == 1], xlab = "Sepal width", ylab = "Petal width", main = "Petal width vs. sepal width", xlim=c(min(x2),max(x2)), ylim=c(min(x4),max(x4)))
points(x2[sample == 2], x4[sample == 2], pch = 2)
points(x2[sample == 3], x4[sample == 2], pch = 3)
legend("topright", legend = c("Iris setosa", "Iris versicolor", "Iris virginica"), pch = c(1, 2, 3))
xpop1 <- cbind(x2[sample==1],x4[sample==1])
xpop2 <- cbind(x2[sample==2],x4[sample==2])
xpop3 <- cbind(x2[sample==3],x4[sample==3])
xpop1bar <- colMeans(xpop1)
xpop2bar <- colMeans(xpop2)
xpop3bar <- colMeans(xpop3)
xpop1var <- var(xpop1)
xpop2var <- var(xpop2)
xpop3var <- var(xpop3)
x0 <- matrix(c(3.5, 1.75), nrow = 2)
d1x0 <- -.5 * log(det(xpop1var)) - .5 %*% t(x0 - xpop1bar) %*% solve(xpop1var) %*% (x0 - xpop1bar) + log(1/3)
d1x0
d2x0 <- -.5 * log(det(xpop2var)) - .5 %*% t(x0 - xpop2bar) %*% solve(xpop2var) %*% (x0 - xpop2bar) + log(1/3)
d2x0
d3x0 <- -.5 * log(det(xpop3var)) - .5 %*% t(x0 - xpop3bar) %*% solve(xpop3var) %*% (x0 - xpop3bar) + log(1/3)
d3x0
n1 <- 50
n2 <- 50
n3 <- 50
Spool <- 1/(n1 + n2 + n3 - 3) * (49 * xpop1var + 49 * xpop2var + 49 * xpop3var) 
d1x0 <- t(xpop1bar) %*% solve(Spool) %*% x0 - .5 %*% t(xpop1bar) %*% solve(Spool) %*% xpop1bar + log(1/3)
d1x0
d2x0 <- t(xpop2bar) %*% solve(Spool) %*% x0 - .5 %*% t(xpop2bar) %*% solve(Spool) %*% xpop2bar + log(1/3)
d2x0
d3x0 <- t(xpop3bar) %*% solve(Spool) %*% x0 - .5 %*% t(xpop3bar) %*% solve(Spool) %*% xpop3bar + log(1/3)
d3x0
d12x0 <- t(xpop1bar - xpop2bar) %*% solve(Spool) %*% x0 - .5 %*% t(xpop1bar - xpop2bar) %*% solve(Spool) %*% (xpop1bar + xpop2bar)
d12x0
d13x0 <- t(xpop1bar - xpop3bar) %*% solve(Spool) %*% x0 - .5 %*% t(xpop1bar - xpop3bar) %*% solve(Spool) %*% (xpop1bar + xpop3bar)
d13x0
d21x0 <- t(xpop2bar - xpop1bar) %*% solve(Spool) %*% x0 - .5 %*% t(xpop2bar - xpop1bar) %*% solve(Spool) %*% (xpop2bar + xpop1bar)
d21x0
d23x0 <- t(xpop2bar - xpop3bar) %*% solve(Spool) %*% x0 - .5 %*% t(xpop2bar - xpop3bar) %*% solve(Spool) %*% (xpop2bar + xpop3bar)
d23x0
d31x0 <- t(xpop3bar - xpop1bar) %*% solve(Spool) %*% x0 - .5 %*% t(xpop3bar - xpop1bar) %*% solve(Spool) %*% (xpop3bar + xpop1bar)
d31x0
d32x0 <- t(xpop3bar - xpop2bar) %*% solve(Spool) %*% x0 - .5 %*% t(xpop3bar - xpop2bar) %*% solve(Spool) %*% (xpop3bar + xpop2bar)
d32x0
miss1 <- 0
for (i in 1:50)
{
	d1x0 <- t(xpop1bar) %*% solve(Spool) %*% xpop1[i, ] - .5 %*% t(xpop1bar) %*% solve(Spool) %*% xpop1bar + log(1/3)
	d2x0 <- t(xpop2bar) %*% solve(Spool) %*% xpop1[i, ] - .5 %*% t(xpop2bar) %*% solve(Spool) %*% xpop2bar + log(1/3)
	d3x0 <- t(xpop3bar) %*% solve(Spool) %*% xpop1[i, ] - .5 %*% t(xpop3bar) %*% solve(Spool) %*% xpop3bar + log(1/3)
	if(d2x0 > d1x0 | d3x0 > d1x0)
	{
		miss1 <- miss1 + 1
	}
}
miss1
miss2 <- 0
for (i in 1:50)
{
	d1x0 <- t(xpop1bar) %*% solve(Spool) %*% xpop2[i, ] - .5 %*% t(xpop1bar) %*% solve(Spool) %*% xpop1bar + log(1/3)
	d2x0 <- t(xpop2bar) %*% solve(Spool) %*% xpop2[i, ] - .5 %*% t(xpop2bar) %*% solve(Spool) %*% xpop2bar + log(1/3)
	d3x0 <- t(xpop3bar) %*% solve(Spool) %*% xpop2[i, ] - .5 %*% t(xpop3bar) %*% solve(Spool) %*% xpop3bar + log(1/3)
	if(d1x0 > d2x0 | d3x0 > d2x0)
	{
		miss2 <- miss2 + 1
	}
}
miss2
miss3 <- 0
for (i in 1:50)
{
	d1x0 <- t(xpop1bar) %*% solve(Spool) %*% xpop3[i, ] - .5 %*% t(xpop1bar) %*% solve(Spool) %*% xpop1bar + log(1/3)
	d2x0 <- t(xpop2bar) %*% solve(Spool) %*% xpop3[i, ] - .5 %*% t(xpop2bar) %*% solve(Spool) %*% xpop2bar + log(1/3)
	d3x0 <- t(xpop3bar) %*% solve(Spool) %*% xpop3[i, ] - .5 %*% t(xpop3bar) %*% solve(Spool) %*% xpop3bar + log(1/3)
	if(d1x0 > d3x0 | d2x0 > d3x0)
	{
		miss3 <- miss3 + 1
	}
}
miss3
aper <- (miss1 + miss2 + miss3)/150
aper
