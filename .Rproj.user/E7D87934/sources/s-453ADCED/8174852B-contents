XtX_inv = matrix(c(29.7289, 0.0722, -1.9926, 0.0722, 0.00037, -0.00555,-1.9926,-0.00555, 0.13631), nrow = 3)
X_tY = c(3820,249634,66073)
YtY = 721072.4


YtY - 2 * t(X_tY)%*%XtX_inv%*%X_tY + t(X_tY)%*%XtX_inv%*%X_tY

XtX_inv%*%X_tY


t(c(1,60,20))%*%c(69.08700, 1.46343, 9.20993) - sqrt(qf(0.95,1,21-3)) * sqrt(618.6194)*sqrt(t(c(1,60,20)) %*% XtX_inv %*% c(1,60,20))


t(c(1,60,20))%*%c(69.08700, 1.46343, 9.20993) + sqrt(qf(0.95,1,21-3)) * sqrt(618.6194)*sqrt(t(c(1,60,20)) %*% XtX_inv %*% c(1,60,20))


mu1 = c(29, 42, 38, 40, 43, 40, 30, 42)
mu2 = c(30, 35, 39, 28, 31, 31, 29, 35, 29, 33)
mu3 = c(26, 32, 21, 20, 23, 22)

sd(mu1)
sd(mu2)
sd(mu3)

Y = c(mu1, mu2, mu3)
group = rep(c("a","b","c"), c(8, 10, 6))
lm = lm(Y~group-1)
summary(lm)

X =model.matrix(lm)
solve(t(X)%*%X)%*%t(X)%*%Y

L = matrix(c(1,-1,0,1,0,-1), nrow = 3)
t(L)%*%solve(t(X)%*%X)%*%L



require('multcomp')

K = t(L)
K = matrix(c(1,-1,0,1,0,-1,0,1,-1,1,-2,1), byrow = TRUE, ncol =3)
K = rbind(t(c(0,1,0)),t(c(1,0,0)))

# rownames(K) = names(coef(lm)) [-1]
lm_lh = glht(lm, K)
lm_bf = summary(lm_lh, test= adjusted(type = "bonferroni")) 
confint(lm_bf, level = 0.95)



