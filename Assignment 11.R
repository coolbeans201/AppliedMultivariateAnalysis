radiotherapy <- read.table("C:\\Users\\Beans\\Desktop\\BRIDGE MAN\\Junior Year\\Spring Semester\\Applied Multivariate Analysis\\Data\\T1-7.DAT", header = F, col.names = c("x1", "x2", "x3", "x4", "x5", "x6"))
attach(radiotherapy) 
n <- 98
sdat <- (radiotherapy - matrix(rep(1,n), nrow = n) %*% colMeans(radiotherapy)) %*% diag(1 / sqrt(diag(cov(radiotherapy))))
pr <- princomp(sdat)
summary(pr)
pr$loadings[,1:6]
screeplot(pr, type = "lines")
cor(pr$scores, thematrix)
paper <- read.table("C:\\Users\\Beans\\Desktop\\BRIDGE MAN\\Junior Year\\Spring Semester\\Applied Multivariate Analysis\\Data\\T7-7.DAT", header = F, col.names = c("y1", "y2", "y3", "y4", "z1", "z2", "z3", "z4"))
attach(paper) 
paper1 <- paper[ , 1:4]
paper2 <- paper[ , 5:8]
cc <- cancor(paper1, paper2)
U <- as.matrix(paper1) %*% as.matrix(cc$xcoef)
V <- as.matrix(paper2) %*% as.matrix(cc$ycoef)
cor(U, paper1)
cor(U, paper2)
cor(Y, paper1)
cor(Y, paper2)
