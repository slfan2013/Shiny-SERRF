# library(nlme)
# per = c()
# for(i in 1:100){
#   id = c(rep(1,10), 2:100)
#   y = rnorm(length(id))
#   y[1:10] = rnorm(10, sd = 0.5)
#   mm = lme(y~1, random = ~1|id)
#   
#   x = as.numeric(VarCorr(mm)[1:2])
#   per[i] = x[1]/sum(x)
# }
# 
# 
# library(nlme)
# per2 = c()
# for(i in 1:100){
#   id = c(rep(1,10), 2:100)
#   y = rnorm(length(id))
#   y[1:10] = rnorm(10, sd = 0.5)+20
#   mm = lme(y~1, random = ~1|id)
#   
#   x = as.numeric(VarCorr(mm)[1:2])
#   per2[i] = x[1]/sum(x)
# }
# 
# 
# 
# 
# # per2
# per2 = c()
# for(i in 1:100){
#   id = c(rep(1,100), 2:1000)
#   y = rnorm(length(id))
#   y[1:100] = rnorm(100, sd = 0.5)
#   mm = lme(y~1, random = ~1|id)
#   
#   x = as.numeric(VarCorr(mm)[1:2])
#   per2[i] = x[1]/sum(x)
# }
# 
# 
# 
# # per2
# median(per)
# median(per2)
# # median(per3)
# 
# boxplot( (per), (per2), ylab = 'ICC', xlab = "small vs large", sub = "sample size")
