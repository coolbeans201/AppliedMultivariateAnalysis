dataset <- Prostate[c(1:67),]
model1 <- lm(dataset$lpsa ~ dataset$lweight + dataset$age + dataset$lcavol + dataset$lbph + dataset$svi + dataset$lcp + dataset$gleason + dataset$pgg45)
summary(model1)
model2 <- lm(dataset$lpsa ~ 1)
step(model2, scope = list(lower = model2, upper = model1), direction = "forward")
model3 <- lm(lpsa ~ lweight + age + lcavol + lbph + lcp + gleason, dataset)
summary(model3)
resid <- model3$residuals
plot(dataset$lpsa, resid)
dataset2 <- Prostate[c(68:97),]
predict(model3, dataset2)
resid2 <- dataset2$lpsa - predict(model3, dataset2) 
mean(resid2)
vif(model3)
select(lm.ridge(lpsa ~ ., lambda = seq(0,1,0.001), data = dataset))
model4 <- lm.ridge(lpsa ~ ., dataset, lambda = 1)
Z.matrix <- cbind(rep(1,30), dataset2$lweight, dataset2$age, dataset2$lcavol, dataset2$lbph, dataset2$svi, dataset2$lcp,dataset2$gleason,dataset2$pgg45)
fitted.vals <- Z.matrix %*% c(0.066773181, 0.447546149, 0.358744258, -0.023998676, 0.176656029, -0.279600710, -0.098865155, 0.232846662, 0.003380113)
resid3 <- dataset2$lpsa - fitted.vals
mean(resid3)
Z.matrix2 <- cbind(rep(1,67), dataset$lweight, dataset$age, dataset$lcavol, dataset$lbph, dataset$svi, dataset$lcp,dataset$gleason,dataset$pgg45)
fit = glmnet(Z.matrix2, dataset$lpsa, alpha = 1)
plot(fit, xvar = "lambda", label = TRUE) # Solution path
cvfit = cv.glmnet(Z.matrix2, dataset$lpsa, alpha = 1)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, dataset2)
Z.matrix3 <- cbind(rep(1,30), dataset2$lweight, dataset2$age, dataset2$lcavol, dataset2$lbph, dataset2$svi, dataset2$lcp,dataset2$gleason,dataset2$pgg45)
fitted.vals2 <- Z.matrix3 %*% c(0.062420004, 0.351714686, -.022996056, .445623165, .174173570, -0.257618053, -0.091823988, 0.229571567, 0.002999528)
resid4 <- dataset2$lpsa - fitted.vals2
mean(resid4)
