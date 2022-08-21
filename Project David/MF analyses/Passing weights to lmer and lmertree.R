## linear mixed models - reference values from older code
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, weights = rep(0.1, times = nrow(sleepstudy)))
summary(fm1)# (with its own print method; see class?merMod % ./merMod-class.Rd


## linear mixed models - reference values from older code
fm2 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm2)# (with its own print method; see class?merMod % ./merMod-class.Rd


## linear mixed models - reference values from older code
sleepstudy3 <- rbind(sleepstudy, sleepstudy, sleepstudy)
fm3 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy3)
summary(fm3)# (with its own print method; see class?merMod % ./merMod-class.Rd


VarCorr(fm1)
VarCorr(fm2)
VarCorr(fm3)


## Weighted versus unweighted
cor(ranef(fm1)$Subject[,1], ranef(fm2)$Subject[,1])
cor(ranef(fm1)$Subject[,2], ranef(fm2)$Subject[,2])

## unweighted versus 3 times multiplied
cor(ranef(fm2)$Subject[,1], ranef(fm3)$Subject[,1])
cor(ranef(fm2)$Subject[,2], ranef(fm3)$Subject[,2])

## Weighted versus unweighted
cor(ranef(fm1)$Subject[,1], ranef(fm2)$Subject[,1])
cor(ranef(fm1)$Subject[,2], ranef(fm2)$Subject[,2])

## unweighted versus 3 times multiplied
cor(fixef(fm2), fixef(fm1))
cor(fixef(fm2), fixef(fm3))






library(glmertree)

gt1 <- lmertree(Reaction ~ 1 | (1 | Subject) | Days, data = sleepstudy, 
                 weights = rep(0.5, times = nrow(sleepstudy)), alpha = .99)
fixef(gt1)
library(strucchange)
sctest(gt1$tree)
gt2 <- lmertree(Reaction ~ 1 | (1 | Subject) | Days, data = sleepstudy)
fixef(gt2)
sctest(gt2$tree)
sctest(gt3$tree)
ranef(gt1)
ranef(gt2)
ranef(gt3)
## tests are different, ranef are identical


gt3 <- lmertree(Reaction ~ 1 | (1 | Subject) | Days, data = sleepstudy3)
fixef(gt3)


gt4 <- lmertree(Reaction ~ 1 | (1 | Subject) | Days, data = sleepstudy3,
                weights = rep(1/3, times = nrow(sleepstudy)))
fixef(gt4)
fixef(gt2)
plot(ranef(gt2), ranef(gt4))
plot(gt2$tree)
plot(gt4$tree)
