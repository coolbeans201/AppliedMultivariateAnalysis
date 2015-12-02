iris <- read.table("C:\\Users\\Beans\\Desktop\\BRIDGE MAN\\Junior Year\\Spring Semester\\Applied Multivariate Analysis\\Data\\T11-5.dat",header=F,col.names=c("x1","x2","x3","x4","x5"))
attach(iris)
x <- cbind(x2,x4)
x5 <- factor(x5)
iris.mov1 <- manova(x ~ x5)
summary(iris.mov1, test = "Wilks")
subset1 <- subset(x, x5 == 1)
subset2 <- subset(x, x5 == 2)
subset3 <- subset(x, x5 == 3)
S1 <- var(subset1)
S2 <- var(subset2)
S3 <- var(subset3)
n1 <- 50  
n2 <- 50 
n3 <- 50 
g <- 3  
p <- 2
Sp <- 1 / (n1 - 1 + n2 - 1 + n3 - 1) * ((n1 - 1) * S1 + (n2 - 1) * S2 + (n3 - 1) * S3)
u <- (1 / (n1 - 1) + 1 / (n2 - 1) + 1 / (n3 - 1) - 1 / (n1 - 1 + n2 - 1 + n3 - 1)) * (2 * p ^ 2 + 3 * p - 1) / (6 * (p + 1) * (g - 1))
C <- (1 - u) * ((n1 - 1 + n2 - 1 + n3 - 1) * log(det(Sp)) - (n1 - 1) * log(det(S1)) - (n2 - 1) * log(det(S2)) - (n3 - 1) * log(det(S3)))
C
qchisq(0.95, p * (p + 1) * (g - 1) / 2)
peanuts <- read.table("C:\\Users\\Beans\\Desktop\\BRIDGE MAN\\Junior Year\\Spring Semester\\Applied Multivariate Analysis\\Data\\T6-17.dat",header=F,col.names=c("x1","x2","x3","x4","x5"))
attach(peanuts)
x <- cbind(x3, x4, x5)
x1 <- factor(x1)
x2 <- factor(x2)
peanuts.mov1 <- manova(x ~ x1 * x2)
summary(peanuts.mov1, test = "Wilks")
residuals <- resid(peanut.mov1)
resid1 <- c(.5, -.5, 4.65, -4.65, 3.55, -3.55, 2.55, -2.55, 3.25, -3.25, .75, -.75)
resid2 <- c(-7.30, 7.30, 9.20, -9.20, -4.60, 4.60, 2.15, -2.15, -0.30, -0.30, -3.50, 3.50)
resid3 <- c(-1.15, 1.15, 5.55, -5.55, 2, -2, 3.15, -3.15, -0.4, .4, -1.10, 1.10)
qqnorm(resid1); qqline(resid1)
qqnorm(resid2); qqline(resid2)
qqnorm(resid3); qqline(resid3)
peanuts.aov1 <- aov(x3 ~ x1*x2)
summary(peanuts.aov1)
peanuts.aov2 <- aov(x4 ~ x1*x2)
summary(peanuts.aov2)
peanuts.aov3 <- aov(x5 ~ x1*x2)
summary(peanuts.aov3)


