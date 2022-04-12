library("haven")
library("dbarts")
library("glmertree")
library("dplyr")
library("here")
library("multilevel")
library("mice")

###################################################################
# Bayesian Data generation
gendata_bay <- function(data, PPD, n_gen){
  
  if(nrow(data) != ncol(PPD)){stop("Amount of observations in PPD is different from data")}
  
  index_gen <- sample.int(n = nrow(data), size = n_gen, replace = TRUE)
  x_gen <- data[index_gen,]
  # y_gen <- sapply(index_gen, function(i) sample(x = PPD[,i], size = 1))
  y_gen <- sapply(index_gen, function(i) sample(x = PPD[,i][PPD[,i] >= quantile(PPD[,i], probs = .40) & PPD[,i] <= quantile(PPD[,i], probs = .60)], size = 1))
  
  return(cbind(x_gen, y_gen))
}



data <- runif(100, 0, 500)



sapply(index_gen, function(i) sample(x = PPD[,i], size = 1))

PPD[,i][PPD[,i] >= quantile(PPD[,i], probs = .25) & PPD[,i] <= quantile(PPD[,i], probs = .75)]

summary(data[data >= quantile(data, probs = .25) & data <= quantile(data, probs = .75)])
summary(data)

###################################################################
# Load safety data and split
safety = read_sav(file.path(here(), "Safety.sav"))

set.seed(420)
sample <- sample.int(n = nrow(safety), size = floor(.75*nrow(safety)), replace = F)
safety_train <- safety[sample, ]
safety_test  <- safety[-sample, ]

###################################################################
# Bart
bart_saf <- bart2(unsafe ~ age + sex + economic + crowded,
                  data = safety_train, n.trees = 200, keepTrees = TRUE,
                  seed = 420, verbose = TRUE)

PPD_bart <- predict(bart_saf, newdata = safety_test, type = "ppd")
median_bart <- apply(PPD_bart, 2, median)

MSE_bart <- mean((median_bart - safety_test$unsafe)^2)
MSE_bart


###################################################################
# Multilevel Bart
Mbart_saf <- rbart_vi(unsafe ~ age + sex + economic + crowded,
         data = safety_train, group.by = safety_train$street,
         test = safety_test, group.by.test = safety_test$street,
         n.trees = 100, keepTrees = TRUE, seed = 420)

dim(safety_train)
length(safety_train$street)

PPD_mlbart <- predict(Mbart_saf, newdata = safety_test, type = "ppd",
                  group.by = safety_test$street, combineChains = TRUE)
median_mlbart <- fitted(object = Mbart_saf, type = "ppd", sample = "test")

MSE_mlbart <- mean((median_mlbart - safety_test$unsafe)^2)
MSE_mlbart

mean_mlbart <- (apply(X = PPD_mlbart, MARGIN = 2, FUN = mean))
MSE_mlbart <- mean((mean_mlbart - safety_test$unsafe)^2)
MSE_mlbart


MSE_mlbart <- mean((rep(x = 1 , times = nrow(safety_test)) - safety_test$unsafe)^2)

table(safety_test$unsafe)
var(safety_test$unsafe)


###################################################################
# Born-again lmtree (BART)
PPD_bart_train <- predict(bart_saf, newdata = safety_train, type = "ppd")
surr_bart <- gendata_bay(safety_train, PPD_bart_train, 1000)

babart_saf <- lmtree(y_gen ~ 1 | age + sex + economic + crowded,
           data = surr_bart, weights = rep(nrow(safety_train)/nrow(surr_bart), times = nrow(surr_bart)))

plot(babart_saf$tree, gp = gpar(cex = .3))

summary(babart_saf)


###################################################################
# Born-again lmertree (Multilevel BART)
PPD_mlbart_train <- predict(Mbart_saf, newdata = safety_train, type = "ppd",
                      group.by = safety_train$street, combineChains = TRUE)

surr_mlbart <- gendata_bay(safety_train, PPD_mlbart_train, 10000)
surr_mlbart$y_gen <- as.factor(sapply(round(surr_mlbart$y_gen), function(i) {if(i < 1){i=1} else if(i > 3){i=3} else{i=i}}))

table(surr_mlbart$y_gen)


babart_saf <- lmertree(y_gen ~ 1 | (1|street) | age + sex + economic + crowded,
                       data = surr_mlbart)

plot(babart_saf)
babart_saf

pred_babart_saf <- predict(babart_saf, newdata = safety_test)
MSE_ba <- mean((pred_babart_saf - safety_test$unsafe)^2)
MSE_ba
var(safety_test$unsafe)


plot(x = PPD_median, y = safety_test$unsafe, xlim = c(0.5,3), xlab = "predicted", ylab = "observed", main = "BART")
abline(a = 0, b = 1)

###################################################################
# GLMM tree
glmtree_saf <- lmertree(unsafe ~ 1 | (1|street) | age + sex + economic + crowded,
                       data = safety_train)

pred_glmtree_saf <- predict(glmtree_saf, newdata = safety_test)
MSE_glmtree_saf <- mean((pred_glmtree_saf - safety_test$unsafe)^2)
MSE_glmtree_saf
var(safety_test$unsafe)


plot(glmtree_saf$tree)
glmtree_saf
(length(glmtree_saf$tree) - 1 ) / 2

                       
## RQ1: Does born-again approach improve performance of mixed-effects trees?
## RQ2: Do we need to multiply the original dataset / generate a very large number of observations for the artificially generated dataset?
## RQ3: If yes: Do we need to adjust the weights for multiplying the data?                       
                       
                       
###################################################################
# Performance

MSE_bart
MSE_mlbart


par(mfrow=c(1,2))
plot(x = PPD_median, y = safety_test$unsafe, xlim = c(0.5,3), xlab = "predicted", ylab = "observed", main = "BART")
abline(a = 0, b = 1)
plot(x = PPD_ml, y = safety_test$unsafe, xlim = c(0.5,3), xlab = "predicted", ylab = "observed", main = "ml BART")
abline(a = 0, b = 1)

############################################################################
# univbct ##################################################################
############################################################################

###################################################################
# Load safety data and split
#data(univbct)
View(univbct)
head(univbct)


# impute missing data
imputed_data <- mice(data = univbct, m = 20, maxit = 50, seed = 500, printFlag = TRUE)
univbct <- mice::complete(data=imputed_data)

set.seed(420)
sample <- sample.int(n = nrow(univbct), size = floor(.75*nrow(univbct)), replace = FALSE)
traindat <- univbct[sample, ]
testdat  <- univbct[-sample, ]

dim(traindat)
length(traindat$COMPANY)
dim(testdat)
length(testdat$COMPANY)

table(testdat$COMPANY)
table(traindat$COMPANY)

class(univbct)

univbct <- as_tibble(univbct)


###################################################################
# Multilevel Bart

Mbart_saf <- rbart_vi(JSAT ~ MARITAL + GENDER + HOWLONG + RANK + EDUCATE + AGE + TIME + COMMIT + READY,
                      data = traindat, group.by = traindat$COMPANY,
                      test = testdat, group.by.test = testdat$COMPANY,
                      n.trees = 100, keepTrees = TRUE, seed = 420)

PPD_mlbart_jobsat <- predict(Mbart_saf, newdata = testdat, type = "ppd",
                      group.by = testdat$COMPANY, combineChains = TRUE)
median_mlbart_jobsat <- fitted(object = Mbart_saf, type = "ppd", sample = "test")

MSE_mlbart <- mean((median_mlbart_jobsat - testdat$JSAT)^2)
MSE_mlbart
var(testdat$JSAT)

###################################################################
# Born-again lmertree (Multilevel BART)
PPD_mlbart_train <- predict(Mbart_saf, newdata = traindat, type = "ppd",
                            group.by = traindat$COMPANY, combineChains = TRUE)

surr_mlbart <- gendata_bay(traindat, PPD_mlbart_train, nrow(traindat)*50)

babart_saf <- lmertree(y_gen ~ TIME | (1|COMPANY) | MARITAL + GENDER + HOWLONG + RANK + EDUCATE + AGE + COMMIT + READY,
                       data = surr_mlbart)

pred_babart_saf <- predict(babart_saf, newdata = testdat)
MSE_ba <- mean((pred_babart_saf - testdat$JSAT)^2)
MSE_ba

plot(babart_saf)
babart_saf
mean(surr_mlbart$JSAT)
length(babart_saf$tree)

plot(x = PPD_median, y = safety_test$unsafe, xlim = c(0.5,3), xlab = "predicted", ylab = "observed", main = "BART")
abline(a = 0, b = 1)


###################################################################
# GLMM tree
babart_saf <- lmertree(JSAT ~ TIME | (1|COMPANY) | MARITAL + GENDER + HOWLONG + RANK + EDUCATE + AGE + COMMIT + READY,
                       data = traindat)

pred_babart_saf <- predict(babart_saf, newdata = testdat)
MSE_ba <- mean((pred_babart_saf - testdat$JSAT)^2)
MSE_ba

plot(babart_saf$tree)
babart_saf
(length(babart_saf$tree) - 1 ) / 2


## RQ1: Does born-again approach improve performance of mixed-effects trees?
## RQ2: Do we need to multiply the original dataset / generate a very large number of observations for the artificially generated dataset?
## RQ3: If yes: Do we need to adjust the weights for multiplying the data?                       




###################################################################
# Performance

MSE_bart
MSE_mlbart


par(mfrow=c(1,2))
plot(x = PPD_median, y = safety_test$unsafe, xlim = c(0.5,3), xlab = "predicted", ylab = "observed", main = "BART")
abline(a = 0, b = 1)
plot(x = PPD_ml, y = safety_test$unsafe, xlim = c(0.5,3), xlab = "predicted", ylab = "observed", main = "ml BART")
abline(a = 0, b = 1)


############################################################################
# Gendata ##################################################################
############################################################################


set.seed(1234) #for reproducability

N <- 1000 #sample size
nG <- 20 #anount of groups

G <- factor(sample(1:C, N, replace=T)) #grouping variable

# level 1 coefs
beta1 <- 1
beta2 <- -2
beta3 <- 3
beta4 <- -4
beta5 <- -1
beta6 <- 2
beta7 <- 3

# level 2 coefs
beta_l2 <- 2 

# level 1 vars
x1 <- rnorm(N)
x2 <- rnorm(N)
x3 <- rnorm(N)
err1 <- rnorm(N)

# level 2 vars
tmp <- rnorm(nG)#generate 20 random numbers, m = 0, sd = 1
group <- sapply(G, function(i) tmp[i]) #all units in l2 have the same value
tmp <- rnorm(nG) #error term for level 2
err2 <- sapply(G, function(i) tmp[i]) #all units in l2 have the same value

LETTERS

y <- beta1*x1 + beta2*x2 + beta3*x3 + 
  beta4*x1*x2 + beta5*x2*x3 + beta6*x1*x3 + beta7*x1*x2*x3 +
  beta_l2*group + 
  err1 + err2

#putting it all together
simdat <- data.frame(y, group, x1, x2, x3)

# y <- beta1*x1 + beta2*x2 + beta3*x3 + 
#   beta4*x1*x2 + beta5*x2*x3 + beta6*x1*x3 + beta7*x1*x2*x3 +
#   err1

# Mbart <- bart2(y ~ x1 + x2 + x3,
#           data = traindat,
#           test = testdat,
#           n.trees = 100, keepTrees = TRUE, seed = 420)
# PPD_mlbart <- predict(Mbart, newdata = testdat, type = "ppd",
#                      combineChains = TRUE)
# 
# PPD_mlbart_train <- predict(Mbart, newdata = traindat, type = "ppd",
#                             combineChains = TRUE)
# surr_mlbart <- gendata_bay(traindat, PPD_mlbart_train, nrow(traindat)*2)
# 
# babart <- tree(y_gen ~ x1 + x2 + x3,
#                    data = surr_mlbart)
# babart <- tree(y ~ x1 + x2 + x3,
#                data = traindat)
# 
# pred_babart <- predict(babart, newdata = testdat)
# MSE_ba <- mean((pred_babart - testdat$y)^2)
# MSE_ba
# var(traindat$y)
# plot(babart)
# babart
# mean(surr_mlbart$y)
# length(babart$tree)

set.seed(420)
sample <- sample.int(n = nrow(simdat), size = floor(.75*nrow(simdat)), replace = FALSE)
traindat <- simdat[sample, ]
testdat  <- simdat[-sample, ]

dim(traindat)
length(traindat$y)
dim(testdat)
length(testdat$y)

table(testdat$y)
table(traindat$y)

class(simdat)

simdat <- as_tibble(simdat)


###################################################################
# Multilevel Bart

Mbart <- rbart_vi(y ~ x1 + x2 + x3,
                      data = traindat, group.by = traindat$group,
                      test = testdat, group.by.test = testdat$group,
                      n.trees = 100, keepTrees = TRUE, seed = 420)

PPD_mlbart <- predict(Mbart, newdata = testdat, type = "ppd",
                             group.by = testdat$group, combineChains = TRUE)
median_mlbart <- fitted(object = Mbart, type = "ppd", sample = "test")

MSE_mlbart <- mean((median_mlbart - testdat$y)^2)
MSE_mlbart
var(testdat$y)

###################################################################
# Born-again lmertree (Multilevel BART)
PPD_mlbart_train <- predict(Mbart, newdata = traindat, type = "ppd",
                            group.by = traindat$group, combineChains = TRUE)

surr_mlbart <- gendata_bay(traindat, PPD_mlbart_train, nrow(traindat)*5)

babart <- lmertree(y_gen ~ 1 | (1|group) | x1 + x2 + x3,
                       data = surr_mlbart)

pred_babart <- predict(babart, newdata = testdat)
MSE_ba <- mean((pred_babart - testdat$y)^2)
MSE_ba
var(testdat$y)

plot(babart_saf)
babart_saf
mean(surr_mlbart$y)
length(babart_saf$tree)

plot(x = pred_babart, y = testdat$y, xlab = "predicted", ylab = "observed", main = "ba-BART")
abline(a = 0, b = 1)


###################################################################
# GLMM tree
babart <- lmertree(y ~ 1 | (1|group) | x1 + x2 + x3,
                       data = traindat)

pred_babart <- predict(babart, newdata = testdat)
MSE_glmtree <- mean((pred_babart - testdat$y)^2)
MSE_glmtree
var(testdat$y)

plot(babart_saf$tree)
babart_saf
(length(babart_saf$tree) - 1 ) / 2


###################################################################
# Performance

MSE_mlbart
MSE_glmtree
MSE_ba
var(testdat$y)


par(mfrow=c(1,2))
plot(x = PPD_median, y = safety_test$unsafe, xlim = c(0.5,3), xlab = "predicted", ylab = "observed", main = "BART")
abline(a = 0, b = 1)
plot(x = PPD_ml, y = safety_test$unsafe, xlim = c(0.5,3), xlab = "predicted", ylab = "observed", main = "ml BART")
abline(a = 0, b = 1)



