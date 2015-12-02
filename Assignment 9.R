realestate <- read.table("C:\\T7-1.dat", header=F, col.names = c("z1", "z2", "y"))
attach(realestate)
estate1 <- lm(y ~ z1 + z2)
summary(estate1)
resid <- residuals(estate1)
plot(z1, resid)
plot(z2, resid)
newdata <- data.frame(z1 = 17, z2 = 46)
predict(estate1, newdata, interval = "prediction", level = 0.95)
estate2 <- lm(y ~ z1)
anova(estate2, estate1)
bones <- read.table("C:\\Users\\Beans\\Desktop\\BRIDGE MAN\\Junior Year\\Spring Semester\\Applied Multivariate Analysis\\Data\\T1-8.dat",header=F,col.names=c("dominantradius","radius","dominanthumerus","humerus","dominantulna","ulna"))
attach(bones)
bones1 <- lm(dominantradius ~ dominantulna + ulna + dominanthumerus + humerus)
bones2 <- lm(dominantradius ~ 1)
step(bones2, scope = list(lower = bones2, upper = bones1), direction = "forward")
bones3 <- lm(dominantradius ~ dominantulna + ulna + dominanthumerus)
summary(bones3)
resid <- residuals(bones3)
plot(dominantulna, resid)
radius1 <- cbind(dominantradius, radius)
bones4 <- lm(radius1 ~ dominantulna + ulna + dominanthumerus + humerus)
summary(bones4)
ssd <- matrix(c(.08804710, .05011946, .05011946, .06260821), nrow = 2, byrow = T)
aic <- 20 * log(det(.05 * ssd))- 2*2*4
aic
z0 <- c(1, -0.5, 3, 6, 1) 
yhat <- z0 %*% m1$coef
yhat
n <- 62 
m <- 4
r <- 4
1 / (n - r - 1) * SSD(m1)$SSD
Z <- cbind(1, dat$V5, dat$V6, dat$V7, dat$V8) 
var.z0 <- t(z0) %*% solve(t(Z) %*% Z) %*% z0
var.z0
m * (n - r - 1) / (n - r - m) * qf(0.95, m, n - r - m)


