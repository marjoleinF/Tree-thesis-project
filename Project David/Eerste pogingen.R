library("haven")
library("dbarts")
library("glmertree")
library("dplyr")
library("here")


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

PPD_mlbart <- predict(Mbart_saf, newdata = safety_test, type = "ppd",
                  group.by = safety_test$street, combineChains = TRUE)
median_mlbart <- fitted(object = Mbart_saf, type = "ppd", sample = "test")

MSE_mlbart <- mean((median_mlbart - safety_test$unsafe)^2)
MSE_mlbart

###################################################################
# Bayesian Data generation
gendata_bay <- function(data, PPD, n_gen){

  if(nrow(data) != ncol(PPD)){stop("Amount of observations in PPD is different from data")}
  
  index_gen <- sample.int(n = nrow(data), size = n_gen, replace = TRUE)
  x_gen <- safety[index_gen,]
  y_gen <- sapply(index_gen, function(i) sample(x = PPD[,i], size = 1))

  return(cbind(x_gen, y_gen))
}

###################################################################
# Born-again lmtree (BART)
PPD_bart_train <- predict(bart_saf, newdata = safety_train, type = "ppd")
surr_bart <- gendata_bay(safety_train, PPD_bart_train, 1000)

babart_saf <- lmtree(y_gen ~ 1 | age + sex + economic + crowded,
           data = surr_bart, weights = rep(nrow(safety_train)/nrow(surr_bart), times = nrow(surr_bart))

plot(babart_saf$tree, gp = gpar(cex = .3))

###################################################################
# Born-again lmertree (Multilevel BART)
PPD_mlbart_train <- predict(Mbart_saf, newdata = safety_train, type = "ppd",
                      group.by = safety_train$street, combineChains = TRUE)
surr_mlbart <- gendata_bay(safety_train, PPD_mlbart_train, 1000)

babart_saf <- lmertree(y_gen ~ 1 | (1|street) | age + sex + economic + crowded,
                       data = surr_mlbart, weights = rep(nrow(safety_train)/nrow(surr_bart), times = nrow(surr_bart))

plot(babart_saf$tree, gp = gpar(cex = .3))

                       
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


