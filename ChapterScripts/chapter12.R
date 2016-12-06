################### Chapter 12 Script
################### 7/25/15 V.1
##################################### Chapter 12 #############################
library(PASWR2)
library(car)
library(scatterplot3d)
library(gridExtra)
library(multcomp)
library(leaps)
library(MASS)
################################ Figure 12.1 #################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mar=c(2, 14, 2, 1), las = 1)
DF <- data.frame(x = c(1, 4, 9), y = c(1, 4, 9))
plot(y~x, data = DF, xaxt = "n", yaxt = "n", 
     xlim = c(0, 12), ylim = c(-2, 12),
     xlab = "", ylab = "", type = "n")
abline(lm(y~x, data = DF), lwd = 2)
axis(side =1, at =c(1, 4, 10), 
     labels = c("$x_1$", "$x_2$", "$x_3$"))
axis(side =2, at =c(1, 4, 10),
     labels = c("$E(Y|x_1) = \\beta_0 + \\beta_1x_1$",
                "$E(Y|x_1) = \\beta_0 + \\beta_1x_1$",
                "$E(Y|x_1) = \\beta_0 + \\beta_1x_1$") )
segments(1, -2, 1, 2.5, lty = "dashed")
segments(0, 1, 1 + 0.75, 1, lty = "dashed")
segments(4, -2, 4, 5.5, lty = "dashed")
segments(0, 4, 4 + 0.75, 4, lty = "dashed")
segments(10, -2, 10, 11.5, lty = "dashed")
segments(0, 10, 10 + 0.75, 10, lty = "dashed")
ys <- seq(-1.5, 1.5, length = 200)
xs <- dnorm(ys, 0, 0.5)
lines(xs + 1, ys + 1, type = "l",lwd = 2)
lines(xs + 4, ys + 4, type = "l",lwd = 2)
lines(xs + 10, ys + 10, type = "l",lwd = 2)
text(7.8, 5.5, "$E(Y|x) = \\beta_0 + \\beta_1x$")
arrows(8, 5.7, 7, 7, length = 0.1, lwd = 2)
par(opar)


################################ R Code 12.1 ###################################

################################ Figure 12.2 ###################################

p <- ggplot(data = GRADES, aes(x = sat, y = gpa))
p + geom_point() + geom_smooth(method = "lm", se = FALSE) + theme_bw()



Y <- GRADES$gpa
x <- GRADES$sat
 
b1 <- sum((x - mean(x)) * (Y - mean(Y))) / sum((x - mean(x))^2)
b0 <- mean(Y) -b1*mean(x)
c(b0, b1)
 
X <- cbind(rep(1, dim(GRADES)[1]), x)
Y <- as.matrix(Y, nrow = dim(GRADES)[1])
betahat <- solve(t(X)%*%X)%*%t(X)%*%Y
beta0hat <- betahat[1, 1]
beta1hat <- betahat[2, 1]
names(beta1hat) <- NULL
c(beta0hat, beta1hat)
 
model.lm <- lm(gpa ~ sat, data = GRADES)
coef(model.lm)
 
b1*50

################################ R Code 12.2 ###################################

hsw.lm <- lm(hwfat ~ abs + triceps, data = HSWRESTLER)
coef(summary(hsw.lm))  # lm coefficients

################################ Figure 12.3 ###################################

library(scatterplot3d)
s3d <- scatterplot3d(x = HSWRESTLER$triceps, y = HSWRESTLER$abs, 
              z = HSWRESTLER$hwfat, xlab = "Tricep Fat Percentage", 
              ylab = "Abdominal Fat Percentage", zlab = "Hydrostatic Fat Percentage",
              box = TRUE, pch = 20, color = "white",
              cex.symbols = 0.75, angle = 60, grid = FALSE)
s3d$plane3d(hsw.lm <- lm(hwfat ~ triceps + abs, data = HSWRESTLER), 
            lty = "dotted", lty.box = "solid")
orig <- s3d$xyz.convert(x = HSWRESTLER$triceps, y = HSWRESTLER$abs, 
                        z = HSWRESTLER$hwfat)
plane <- s3d$xyz.convert(x = HSWRESTLER$triceps, y = HSWRESTLER$abs,  fitted(hsw.lm))
i.negpos <- 1 + (resid(hsw.lm) > 0)
segments(orig$x, orig$y, plane$x, plane$y,
         col = c("darkblue", "lightblue3")[i.negpos])
s3d$points3d(x = HSWRESTLER$triceps, y = HSWRESTLER$abs, 
             z = HSWRESTLER$hwfat,
             col = c("darkblue", "lightblue3")[i.negpos],
             pch = 20)

################################ Example 12.7 ###################################

model.lm <- lm(gpa ~ sat, data = GRADES)
XTXI <- summary(model.lm)$cov.unscaled
MSE <- summary(model.lm)$sigma^2
var.cov.b <- MSE*XTXI
var.cov.b
 
vcov(model.lm)
 
summary(model.lm)$coef  # lm coefficients
 
b0 <- coef(summary(model.lm))[1, 1]
s.b0 <- coef(summary(model.lm))[1, 2]
b1 <- coef(summary(model.lm))[2, 1]
s.b1 <- coef(summary(model.lm))[2, 2]
ct <- qt(1 - 0.10/2, 198)  # alpha = 0.10
CI.B0 <- b0 + c(-1, 1)*ct*s.b0
CI.B1 <- b1 + c(-1, 1)*ct*s.b1

################################ R Code 12.3 ###################################

b0 <- coef(summary(model.lm))[1, 1]
s.b0 <- coef(summary(model.lm))[1, 2]
b1 <- coef(summary(model.lm))[2, 1]
s.b1 <- coef(summary(model.lm))[2, 2]
ct <- qt(1 - 0.10/2, 198)  # alpha = 0.10
CI.B0 <- b0 + c(-1, 1)*ct*s.b0
CI.B0
CI.B1 <- b1 + c(-1, 1)*ct*s.b1
CI.B1
# Or
confint(model.lm, level = 0.90)

################################ Figure 12.4 ###################################

DT <- data.frame(x=c(1,2,3,4,5), Y=c(2,1,4,3,5))
plot(DT$x, DT$Y, pch=19, xlab="$x$", ylab="$Y$", 
     xlim=c(0, 6), ylim=c(0, 6), las=0, cex=1.5,
     type = "n")
points(c(2, 5), c(1, 5), pch = 19, cex = 1.5)
model <- lm(Y~x, data=DT)
arrows(x0 = 1.6, y0 = predict(model, 
                              newdata = data.frame(x = 1.6)), 
       x1 = 5.4, y1 = predict(model, 
                              newdata = data.frame(x = 5.4)),
       length = 0.1, code = 3)  # draw head at both ends
abline(h = 3, lty = "dashed")
yhat <- fitted(model)
points(DT$x[c(2, 5)], yhat[c(2, 5)], pch=22, bg="white", cex=1.5)
shift <- 0.05
segments(2 + shift, 1, 2 + shift, 3, lty = "solid")
text(2.6, 2, "$\\left. \\rule[4.8ex]{0pt}{4ex} \\right\\} Y_i - \\MeanY$")
segments(2 - shift, 1, 2 - shift, yhat[2], lty = "dotted")
text(1.4, 1.55, "$Y_i - \\hat{Y}_i \\left\\{ \\rule{0pt}{4ex} \\right.$")
segments(2 - shift, yhat[2], 2 - shift, 3, lty = "dashed")
text(1.4, 2.6, "$\\hat{Y}_i - \\MeanY \\left\\{ \\rule[1.4ex]{0pt}{2ex} \\right.$")
text(0.5, 3.2, "$\\MeanY = 3$")
segments(5 - shift, 5, 5 - shift, yhat[5], lty = "dotted")
text(4.4, 4.8, "$Y_i - \\hat{Y}_i \\left\\{ \\rule{0pt}{2ex} \\right.$")
segments(5 - shift, yhat[5], 5 - shift, 3, lty = "dashed")
text(4.4, 3.4, "$\\hat{Y}_i - \\MeanY$ \\raisebox{.4cm}{$\\left\\{ \\rule[4.1ex]{0pt}{2ex} \\right.$}")
segments(5 + shift, 5, 5 + shift, 3, lty = "solid")
text(5.6, 4.0, "$\\left. \\rule[4.8ex]{0pt}{3.8ex} \\right\\} Y_i - \\MeanY$")
 
################################ R Code 12.4 ###################################

model.lm <- lm(gpa ~ sat, data = GRADES)
anova(model.lm)  # ANOVA

################################ Figure 12.5 ###################################
set.seed(11)
x <- 1:10
Y1 <- x
Y2 <- x + rnorm(10, 0, 1)
Y3 <- sample(x) + rnorm(10, 0, 0.5)
Y4 <- sample(x) + rnorm(10, 0.5, 1)
DF <- data.frame(x = x, Y1 = Y1, Y2 = Y2, Y3 = Y3, Y4 = Y4)
p1 <- ggplot(data = DF, aes(x = x, y = Y1)) + 
  geom_point() + theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "$x$", y = "$Y$", title = "$R^2 = 1.0$") + 
  xlim(0, 11) + ylim(0, 11)
p2 <- ggplot(data = DF, aes(x = x, y = Y2)) + 
  geom_point() + theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "$x$", y = "$Y$", title = "$R^2 = 0.9145$") + 
  xlim(0, 11) + ylim(0, 11)
p3 <- ggplot(data = DF, aes(x = x, y = Y3)) + 
  geom_point() + theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "$x$", y = "$Y$", title = "$R^2 = 0.044$") + 
  xlim(0, 11) + ylim(0, 11)
p4 <- ggplot(data = DF, aes(x = x, y = Y4)) + 
  geom_point() + theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "$x$", y = "$Y$", title = "$R^2 = 0.011$") + 
  xlim(0, 11) + ylim(0, 11)
library(gridExtra)
grid.arrange(p1, p2, p4, p3, nrow = 2)

################################ R Code 12.5 ###################################

Y <- HSWRESTLER$hwfat
x1 <- HSWRESTLER$abs
x2 <- HSWRESTLER$triceps
mod1 <- lm(Y ~ x1 + x2)
mod2 <- lm(Y ~ x1)
anova(mod1)
anova(mod2)

################################ R Code 12.6 ###################################
SSRx1x2 <- anova(mod1)[1, 2] + anova(mod1)[2, 2]  # SSR(x1, x2)
SSRx1x2
SSRx1 <- anova(mod1)[1, 2]                        # SSR(x1) 
SSRx1
SSRx2Gx1 <- SSRx1x2 - SSRx1                       # SSR(x2 | x1)
SSRx2Gx1
SSEx1 <- anova(mod2)[2, 2]                        # SSE(x1) 
SSEx1
SSEx1x2 <- anova(mod1)[3, 2]                      # SSE(x1, x2) 
SSEx1x2
SSRx2Gx1 <- SSEx1 - SSEx1x2                       # SSR(x2 | x1) 
SSRx2Gx1

################################ R Code 12.7 ###################################

mod1.HSW <- lm(hwfat ~ abs + triceps + subscap, data = HSWRESTLER)
anova(mod1.HSW)  # ANOVA
mod2.HSW <- lm(hwfat ~ subscap + triceps + abs, data = HSWRESTLER)
anova(mod2.HSW)  # ANOVA

################################ R Code 12.8 ###################################

mod3.HSW <- lm(hwfat ~ abs + subscap + triceps, data = HSWRESTLER)
anova(mod3.HSW)  # ANOVA
anova(mod3.HSW)[3, 4]             # Fobs value
coef(summary(mod3.HSW))
coef(summary(mod3.HSW))[4, 3]^2   # tobs value squared

drop1(mod3.HSW, test = "F") # Single term deletions

################################ R Code 12.9 ###################################

mod4.HSW <- lm(hwfat ~ abs + subscap, data = HSWRESTLER)
anova(mod4.HSW) # ANOVA
 
anova(mod4.HSW, mod3.HSW)  # test models

################################ R Code 12.10 ###################################

mod4.HSW <- lm(hwfat ~ subscap, data = HSWRESTLER)
mod5.HSW <- lm(hwfat ~ subscap + abs + triceps, data = HSWRESTLER)
anova(mod4.HSW, mod5.HSW) # test models

################################ R Code 12.11 ###################################
 
K <- matrix(c(0, 0, 1, 0, 0, 0, 0, 1), nrow = 2, byrow = TRUE)
q <- qr(K)$rank           # computes the rank of K
mod5.HSW <- lm(hwfat ~ subscap + abs + triceps, data = HSWRESTLER)
b <- matrix(coef(mod5.HSW), ncol = 1)   # vector of beta hats
m <- matrix(0, byrow = TRUE, nrow = 2)  # right hand side
XTXI <- summary(mod5.HSW)$cov.unscaled  # X'X^-1 matrix
NUM <- t(K%*%b - m)%*%solve(K%*%XTXI%*%t(K))%*%(K%*%b - m)
MSE <- anova(mod5.HSW)[4,3]
Fobs <- NUM/(q*MSE)
pvalue <- pf(Fobs, q, 74, lower = FALSE)
ANS <- c(Fobs = Fobs, pvalue = pvalue)
ANS

################################ R Code 12.12 ###################################

library(car)   # load package car
linearHypothesis(model = mod5.HSW, hypothesis.matrix = K, rhs = m)


################################ R Code 12.13 ###################################
  
K <- matrix(c(0, 2, 1, -1, 0, -5, 0, 1), nrow = 2, byrow =TRUE)
q <- qr(K)$rank                                 # computes the rank of K
mod5.HSW <- lm(hwfat ~ subscap + abs + triceps, data = HSWRESTLER)
b <- matrix(coef(mod5.HSW), ncol = 1)           # vector of beta hats
m <- matrix(c(0, 0.2), byrow = TRUE, nrow = 2)  # right hand side
XTXI <- summary(mod5.HSW)$cov.unscaled          # X'X^-1 matrix
NUM <- t(K%*%b - m)%*%solve(K%*%XTXI%*%t(K))%*%(K%*%b - m)
MSE <- anova(mod5.HSW)[4,3]
Fobs <- NUM/(q*MSE)
pvalue <- pf(Fobs, q, 74, lower = FALSE)
ANS <- c(Fobs = Fobs, pvalue = pvalue)
ANS

################################ R Code 12.14 ###################################

library(multcomp)  # load package multcomp
linearHypothesis(model = mod5.HSW, hypothesis.matrix = K, rhs = m)
summary(glht(model = mod5.HSW, linfct = K, rhs = m), test = Ftest())

################################ R Code 12.15 ###################################
 
K <- matrix(c(0, 0, 1, -1), nrow = 1, byrow =TRUE)
q <- qr(K)$rank                              # computes the rank of K
mod5.HSW <- lm(hwfat ~ subscap + abs + triceps, data = HSWRESTLER)
b <- matrix(coef(mod5.HSW), ncol = 1)        # vector of beta hats
m <- matrix(c(0), byrow = TRUE, nrow = 1)    # right hand side
XTXI <- summary(mod5.HSW)$cov.unscaled       # X'X^-1 matrix
NUM <- t(K%*%b - m)%*%solve(K%*%XTXI%*%t(K))%*%(K%*%b - m)
MSE <- anova(mod5.HSW)[4,3]
Fobs <- NUM/(q*MSE)
pvalue <- pf(Fobs, q, 74, lower = FALSE)
ANS <- c(Fobs = Fobs, pvalue = pvalue)
ANS

################################ R Code 12.16 ###################################

library(multcomp)  # load package multcomp
linearHypothesis(model = mod5.HSW, hypothesis.matrix = K, rhs = m)
summary(glht(model = mod5.HSW, linfct = K, rhs = m), test = Ftest())

################################ R Code 12.17 ###################################

summary(HSWRESTLER)

################################ R Code 12.18 ###################################

library(car)
scatterplotMatrix(x = HSWRESTLER[ , -c(8:9)])  # remove tanfat and skfat
# Or
scatterplotMatrix(formula = ~ hwfat + age + ht + wt + abs + triceps +
                     subscap, data = HSWRESTLER)
 
################################ R Code 12.19 ###################################

cor(HSWRESTLER[, -c(8:9)])  # remove tanfat and skfat 

model.all <- lm(hwfat ~ ., data = HSWRESTLER[, -c(8, 9)])  
drop1(model.all, test = "F") # single term deletions
 
mod.hsw <- update(model.all, .~. - ht)
drop1(mod.hsw, test = "F") # single term deletions
 
mod.hsw <- update(mod.hsw, .~. - subscap)
drop1(mod.hsw, test = "F") # single term deletions
 
mod.hsw <- update(mod.hsw, .~. - wt)
drop1(mod.hsw, test = "F") # single term deletions
 
mod.be <- lm(hwfat ~ age + abs + triceps, data = HSWRESTLER)
summary(mod.be) # lm summary
 
SCOPE <- (~ . + age + ht + wt + abs + triceps + subscap)
mod.fs <- lm(hwfat ~ 1, data = HSWRESTLER)
add1(mod.fs, scope = SCOPE, test = "F")
mod.fs <- update(mod.fs, . ~ . + abs)
add1(mod.fs, scope = SCOPE, test = "F")
mod.fs <- update(mod.fs, . ~ . + triceps)
add1(mod.fs, scope = SCOPE, test = "F")
formula(mod.fs)

################################ R Code 12.20 ###################################

library(leaps)
models <- regsubsets(hwfat ~ ., data = HSWRESTLER[, -c(8, 9)])
summary(models)
R2adj <- summary(models)$adjr2
R2adj
which.max(R2adj)


################################ R Code 12.21 ###################################

MCP <- summary(models)$cp
MCP
which.min(MCP)

################################ R Code 12.22 ###################################

BIC <- summary(models)$bic
BIC
which.min(BIC)

plot(models, scale = "adjr2")
plot(models, scale = "Cp")
plot(models, scale = "bic")

################################ R Code 12.23 ###################################

mod.lm <- lm(hwfat ~ ., data = HSWRESTLER[, -c(8, 9)])
SCOPE <- (~.)
stepAIC(mod.lm, scope = SCOPE, k = 2) 

################################ R Code 12.24 ###################################

mod3.lm <- lm(hwfat ~ age + abs + triceps, data = HSWRESTLER)
X <- model.matrix(mod3.lm)              # n*p design matrix
H <- X%*%solve(t(X)%*%X)%*%t(X)         # H matrix
dim(H)                                  # n by n
hii <- diag(H)                          # extract diagonal values
hii[1:5]                                # show first 5 values  
hat(x = model.matrix(mod3.lm))[1:5]     # show first 5 values  
hatvalues(model = mod3.lm)[1:5]         # show first 5 values  
sum(hii)                                # sum all hii values

################################ Figure 12.9 ###################################

set.seed(2)
n <- 200
y <- 1:n + rnorm(n, 0, 1)
x1 <- y + rnorm(n, 0, 1)
x2 <- y + rnorm(n, 0, 10)*(1:n)/n
x3 <- y + rnorm(n, 0, 10)*(n:1)/n
x4 <- sin((1:n)*pi/30) + rnorm(n, 0, .5)
SD <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
mod1 <- lm(y ~ x1, data = SD)
mod2 <- lm(y ~ x2, data = SD)
mod3 <- lm(y ~ x3, data = SD)
mod4 <- lm(x4 ~ y, data = SD)
mod1f <- fortify(mod1)
mod2f <- fortify(mod2)
mod3f <- fortify(mod3)
mod4f <- fortify(mod4)

p1 <- ggplot(data = mod1f, aes(x = .fitted, y = .resid)) 
P1 <- p1 + geom_point() + 
  theme_bw() + 
  labs(x = "Fitted Values (\\type{mod1})", y = "Residuals", title = "Constant Variance") + 
  geom_hline(yintercept = 0, lty = "dashed")
#
p2 <- ggplot(data = mod2f, aes(x = .fitted, y = .resid)) 
P2 <- p2 + geom_point() + 
  theme_bw() + 
  labs(x = "Fitted Values (\\type{mod2})", y = "Residuals", title = "Increasing Variance") + 
  geom_hline(yintercept = 0, lty = "dashed")
#
p3 <- ggplot(data = mod3f, aes(x = .fitted, y = .resid)) 
P3 <- p3 + geom_point() + 
  theme_bw() + 
  labs(x = "Fitted Values (\\type{mod3})", y = "Residuals", title = "Decreasing Variance") + 
  geom_hline(yintercept = 0, lty = "dashed")
#
p4 <- ggplot(data = mod4f, aes(x = .fitted, y = .resid)) 
P4 <- p4 + geom_point() + 
  theme_bw() + 
  labs(x = "Fitted Values (\\type{mod4})", y = "Residuals", title = "Non-Linear Relationship") + 
  geom_hline(yintercept = 0, lty = "dashed")
########
### arrange all 4 ggplots in one device
library(gridExtra)
grid.arrange(P1, P2, P3, P4, ncol = 2)

################################ R Code 12.25 ###################################

library(car)
durbinWatsonTest(mod4)
shapiro.test(resid(mod4))

################################ Figure 12.10 ###################################

par(mfrow = c(2, 2))
plot(mod1)
par(mfrow = c(1, 1))

################################ Figure 12.11 ###################################

ggplot(data = mod1f, aes(sample = .stdresid)) + 
  stat_qq(size = 1.5) + 
  theme_bw() + 
  geom_abline(intercept = 0, slope = 1)

################################ Example 12.18 ###################################

mod3.hsw <- lm(hwfat ~ age + abs + triceps, data = HSWRESTLER)
fmod <- fortify(mod3.hsw)
head(fmod, n = 3)

################################ R Code 12.26 ###################################

p <- ggplot(data = fmod, aes(x = .fitted, y = .stdresid))
p + geom_point() + geom_hline(yintercept = 0, lty = "dashed") + 
  labs(x = "Fitted Values", y = "Standardized Residuals") +
  theme_bw()

################################ R Code 12.27 ###################################

sort(abs(rstandard(mod3.hsw)), decreasing = TRUE)[1:3]
sort(abs(fmod$.stdresid), decreasing = TRUE)[1:3]

################################ R Code 12.28 ###################################

BCV <- qt(1 - 0.2/(2*78), 73)            # Bonferroni Critcal Value
BCV
sum(abs(rstudent(mod3.hsw)) > BCV)        # how many r values > BCV
max(abs(rstudent(mod3.hsw)))              # value of largest r
which.max(abs(rstudent(mod3.hsw)))        # find row for largest r
outlierTest(mod3.hsw)


################################ R Code 12.29 ###################################

################################ Figure 12.13 ###################################

qqPlot(mod3.hsw, id.n = 3)  

influenceIndexPlot(mod3.hsw, id.n = 3)  # label largest 3 

################################ R Code 12.30 ###################################

mod3.hsw <- lm(hwfat ~ age + abs + triceps, data = HSWRESTLER)
summary(mod3.hsw) # lm summary

################################ R Code 12.31 ###################################

hii.hsw <- hatvalues(mod3.hsw)
hcv <- 2*4/78          # 2*p/n 
hcv
sum(hii.hsw > hcv)     # how many hii.hsw > hcv
which(hii.hsw > hcv)   

################################ R Code 12.32 ###################################

dffitsCV <- 2*sqrt(4/78)         #  2*sqrt(p/n)
dffitsCV
dffits.hsw <- dffits(mod3.hsw)   # DFFITS values  
sum(dffits.hsw > dffitsCV)       # how many dffits.hsw > dffitsCV
which(dffits.hsw > dffitsCV)    

################################ R Code 12.33 ###################################

dfbetasCV <- 2/sqrt(78)  # 2/sqrt(n)
dfbetasCV
dfbetas.hsw <- dfbetas(mod3.hsw)
which(abs(dfbetas.hsw[, 1]) > dfbetasCV)  
which(abs(dfbetas.hsw[, 2]) > dfbetasCV)  
which(abs(dfbetas.hsw[, 3]) > dfbetasCV)  

################################ R Code 12.34 ###################################

study <- c(22, 27, 32, 35, 60)
dfbetas.hsw[study, ]
dffits.hsw[study]
HSWRESTLER[study, ]

################################ R Code 12.35 ###################################

################################ Figure 12.15 ###################################

influencePlot(mod3.hsw, id.n = 3)
 

################################ R Code 12.36 ###################################

inflm.SR <- influence.measures(mod3.hsw)
summary(inflm.SR)  # which observations 'are' influential

################################ R Code 12.37 ###################################

################################ Figure 12.16 ###################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(2, 3))
plot(y ~ x1, data = SIMDATAXT)
with(data = SIMDATAXT,
    lines(x1, x1^.5))             # function y = x1^.5
plot(lm(y ~ x1, data = SIMDATAXT), which = c(1, 2))
plot(y ~ I(x1^.5), data = SIMDATAXT)
mod1 <- lm(y ~ I(x1^.5), data = SIMDATAXT)
abline(mod1)
plot(mod1, which = c(1, 2))
par(opar)

################################ R Code 12.38 ###################################

################################ Figure 12.17 ###################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(2, 3))
plot(y ~ x2, data = SIMDATAXT)
with(data = SIMDATAXT, lines(x2, x2^2))  # function y = x2^2
plot(lm(y ~ x2, data = SIMDATAXT), which = c(1, 2))
plot(y ~ I(x2^2), data = SIMDATAXT)
mod2 <- lm(y ~ I(x2^2), data = SIMDATAXT)
abline(mod2)
plot(mod2, which = c(1, 2))
par(opar)
 

################################ R Code 12.39 ###################################

################################ Figure 12.18 ###################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(2, 3))
plot(y ~ x3, data = SIMDATAXT)
with(data = SIMDATAXT,
    lines(x3, x3^(-1)))          # function Y = 1/x3
plot(lm(y ~ x3, data = SIMDATAXT), which = c(1, 2))
plot(y ~ I(x3^(-1)), data = SIMDATAXT)
mod3 <- lm(y ~ I(x3^(-1)), data = SIMDATAXT)
abline(mod3)
plot(mod3, which = c(1, 2))
par(opar)
 

################################ R Code 12.40 ###################################

modC <- lm(y ~ I(x1^.5) + I(x2^2) + I(x3^(-1)), data = SIMDATAXT)
summary(modC) # lm summary
 
cbind(SIMDATAXT$x2, SIMDATAXT$x3^(-.5))[1:5, ]

################################ R Code 12.41 ###################################
 
modB <- lm(y ~ I(x1^.5) + I(x2^2), data = SIMDATAXT)
X <- model.matrix(modB)
eigen(t(X)%*%X, only.values = TRUE)$values  # extract eigenvalues
lambda.max <- max(eigen(t(X)%*%X, only.values = TRUE)$values)
lambda.min <- min(eigen(t(X)%*%X, only.values = TRUE)$values)
condition.number <- sqrt(lambda.max / lambda.min)
condition.number
 
kappa(X, exact = TRUE)

################################ R Code 12.42 ###################################

VIF1 <- 1/(1 - summary(lm(I(x1^.5) ~ I(x2^2), data = SIMDATAXT))$r.sq)
VIF1
library(car)
VIF2 <- vif(modB)
VIF2

################################ R Code 12.43 ###################################

mod1 <- lm(y ~ I(x1^.5), data = SIMDATAXT)
coef(summary(mod1))  # coefficients mod1
se.x1.mod1 <- coef(summary(mod1))[2,2]
se.x1.modB <- coef(summary(modB))[2,2]
ratio <- se.x1.modB/se.x1.mod1
ratio
sqrt(vif(modB))

#################### For executing R Code 12.44 ###################################

bcPow <- function(U, lambda, jacobian.adjusted=FALSE) {
  bc1 <- function(U, lambda){
    if(any(U[!is.na(U)] <= 0)) stop("First argument must be strictly positive.")
    z <- if (abs(lambda) <= 1.e-6) log(U) else ((U^lambda) - 1)/lambda
    if (jacobian.adjusted == TRUE) {
      z * (exp(mean(log(U), na.rm=TRUE)))^(1-lambda)} else z
  }
  out <- U
  out <- if(is.matrix(out) | is.data.frame(out)){
    if(is.null(colnames(out))) colnames(out) <- 
      paste("Z", 1:dim(out)[2], sep="")
    for (j in 1:ncol(out)) {out[, j] <- bc1(out[, j], lambda[j]) }
    colnames(out) <- paste(colnames(out), round(lambda, 2), sep="^")
    out}  else
      bc1(out, lambda)
  out}
####
BoxCox <- function(object,...) UseMethod("BoxCox")
####
BoxCox.formula <- function (object, lambda = seq(-2, 2, 1/10), 
                            plotit = TRUE, interp = (plotit &&
                            (m < 100)), eps = 1/50, 
                            xlab = expression(lambda), 
                            ylab = "log-Likelihood",
                            family="bcPow",
                            ...)
{
  m <- length(lambda)
  object <- lm(object, y = TRUE, qr = TRUE, ...)
  result <- NextMethod()
  if (plotit)
    invisible(result)
  else result
}
####
BoxCox.lm <- function (object, lambda = seq(-2, 2, 1/10), 
                       plotit = TRUE, interp = (plotit &&
                       (m < 100)), eps = 1/50, 
                       xlab = expression(lambda), 
                       ylab = "log-Likelihood",
                       family="bcPow", ...)
{
  m <- length(lambda)
  if (is.null(object$y) || is.null(object$qr))
    object <- update(object, y = TRUE, qr = TRUE, ...)
  result <- NextMethod()
  if (plotit)
    invisible(result)
  else result
}
####
BoxCox.default <- function(object,
                           lambda = seq(-2, 2, 1/10), 
                           plotit = TRUE, interp = (plotit &&
                           (m < 100)), eps = 1/50, 
                           xlab = expression(lambda), 
                           ylab = "log-Likelihood",
                           family="bcPow", grid=TRUE, ...)
{                
  fam <- match.fun(family)
  if (is.null(object$y) || is.null(object$qr))
    stop(paste(deparse(substitute(object)), "does not have both 'qr' and 'y' components"))
  y <- object$y
  n <- length(y)              
  xqr <- object$qr
  xl <- loglik <- as.vector(lambda)
  m <- length(xl)
  for (i in 1L:m) {
    yt <- fam(y,xl[i],j=TRUE)
    loglik[i] <- -n/2 * log(sum(qr.resid(xqr, yt)^2))
  }
  if (interp) {
    sp <- spline(xl, loglik, n = 100)
    xl <- sp$x
    loglik <- sp$y
    m <- length(xl)
  }
  if (plotit) {
    mx <- (1L:m)[loglik == max(loglik)][1L]
    Lmax <- loglik[mx]
    lim <- Lmax - qchisq(19/20, 1)/2
    plot(xl, loglik, xlab = xlab, ylab = ylab, type = "n",
         ylim = range(loglik, lim))
    if(grid){
      grid(lty=1, equilogs=FALSE)
      box()}
    lines(xl, loglik)
    plims <- par("usr")
    abline(h = lim, lty = 2)
    y0 <- plims[3L]
    scal <- (1/10 * (plims[4L] - y0))/par("pin")[2L]
    scx <- (1/10 * (plims[2L] - plims[1L]))/par("pin")[1L]
    text(xl[1L] + scx, lim + scal, " $95\\%$")
    la <- xl[mx]
    if (mx > 1 && mx < m)
      segments(la, y0, la, Lmax, lty = 2)
    ind <- range((1L:m)[loglik > lim])
    if (loglik[1L] < lim) {
      i <- ind[1L]
      x <- xl[i - 1] + ((lim - loglik[i - 1]) * (xl[i] -
                                                   xl[i - 1]))/(loglik[i] - loglik[i - 1])
      segments(x, y0, x, lim, lty = 2)
    }
    if (loglik[m] < lim) {
      i <- ind[2L] + 1
      x <- xl[i - 1] + ((lim - loglik[i - 1]) * (xl[i] -
                                                   xl[i - 1]))/(loglik[i] - loglik[i - 1])
      segments(x, y0, x, lim, lty = 2)
    }
  }
  list(x = xl, y = loglik)
}

################################ R Code 12.44 ###################################

################################ Figure 12.19 ###################################

library(car)
opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 2))  # split graphics device 1 row 2 columns
modx1 <- lm(y1 ~ x1, data = SIMDATAST)
boxCox(modx1)
boxCox(modx1, lambda = seq(-0.2, 0.2, 0.01))
par(opar)
 

################################ R Code 12.45 ###################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(2, 3))  # split graphics device 2 rows 3 columns


################################ R Code 12.46 ###################################

plot(y1 ~ x1, data = SIMDATAST)

################################ R Code 12.47 ###################################

modx1 <- lm(y1 ~ x1, data = SIMDATAST)
plot(modx1, which = 1)

################################ R Code 12.48 ###################################

boxCox(modx1, lambda = seq(-0.2, 0.2, 0.01))

################################ R Code 12.49 ###################################

plot(log(y1) ~ x1, data = SIMDATAST)

################################ R Code 12.50 ###################################

## plot(lm(log(y1) ~ x1, data = SIMDATAST), which = 1)

################################ R Code 12.51 ###################################

plot(lm(log(y1) ~ x1, data = SIMDATAST), which = 2)
par(opar)
 
################################ R Code 12.52 ###################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(2, 3))  # split graphics device 2 rows 3 columns

################################ R Code 12.53 ###################################

plot(y2 ~ x2, data = SIMDATAST)


################################ R Code 12.54 ###################################

modx2 <- lm(y2 ~ x2, data = SIMDATAST)
plot(modx2, which = 1)


################################ R Code 12.55 ###################################

boxCox(modx2, lambda = seq(-1.4, -0.5, 0.01))

################################ R Code 12.56 ###################################

plot(y2^(-1) ~ x2, data = SIMDATAST)


################################ R Code 12.57 ###################################

plot(lm(y2^(-1) ~ x2, data = SIMDATAST), which = 1)

################################ R Code 12.58 ###################################

plot(lm(y2^(-1) ~ x2, data = SIMDATAST), which = 2)
par(opar)


################################ Figure 12.21 ###################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(2, 3))  # split graphics device 2 rows 3 columns 
plot(y2 ~ x2, data = SIMDATAST, xlab = "$x_2$", ylab = "$y_2$")
modx2 <- lm(y2 ~ x2, data = SIMDATAST)
plot(modx2, which=1)
BoxCox(modx2, lambda = seq(-1.4, -0.5, 0.01), xlab = "$\\lambda$")
plot(y2^(-1) ~ x2, data = SIMDATAST, xlab = "$x_2$", ylab = "$y_2^{-1}$")
plot(lm(y2^(-1) ~ x2, data = SIMDATAST), which = c(1, 2)) 
par(opar)

################################ R Code 12.59 ###################################

################################ Figure 12.22 ###################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(2, 3))  # split graphics device 2 rows 3 columns
plot(y1 ~ x3, data = SIMDATAST, main = "(I)")
mod1 <- lm(y1 ~ x3, data = SIMDATAST)
boxCox(mod1, lambda = seq(-0.2, 0.37, 0.01))
title(main = "(II)")
mod2 <- lm(log(y1) ~ x3, data = SIMDATAST)
plot(log(y1) ~ x3, data = SIMDATAST, main = "(III)")
abline(mod2)
plot(mod2, which = 1, main = "(IV)")
plot(log(y1) ~ I(x3^0.5), data = SIMDATAST, main = "(V)")
mod3 <- lm(log(y1) ~ I(x3^.5), data = SIMDATAST)
abline(mod3)
plot(mod3, which = 1, main ="(VI)")
par(opar)
 

################################ R Code 12.60 ###################################

study <- c(22, 27, 32, 35, 60)  # poorly measured values
set.seed(5)
train <- sample(c(TRUE, FALSE), size = nrow(HSWRESTLER[-study, ]), 
                replace = TRUE, prob = c(0.70, 0.30))
prop.table(table(train))  # compute percent in train
test <- (!train)
prop.table(table(test))   # compute percent in test

################################ R Code 12.61 ###################################

library(leaps)
model.exh <- regsubsets(hwfat ~ ., data = HSWRESTLER[train, 1:7], 
                        method = "exhaustive")
summary(model.exh)

################################ R Code 12.62 ###################################

test.mat <- model.matrix(hwfat ~., data = HSWRESTLER[test, 1:7])
val.errors <- rep(NA, 6)
for(i in 1:6){
  coefi = coef(model.exh, id = i)
  pred <- test.mat[, names(coefi)]%*%coefi
  val.errors[i] = mean((HSWRESTLER[test, ]$hwfat - pred)^2)
}
val.errors
coef(model.exh, 3)  # coefficients for model with 3 predictors

################################ R Code 12.63 ###################################

regfit.best <- regsubsets(hwfat ~ ., data = HSWRESTLER[-study, 1:7])
coef(regfit.best, 3) # coef for best 3 predictor model
mod.VSA <- lm(hwfat ~ wt + abs + triceps, 
                data = HSWRESTLER[-study, 1:7])
summary(mod.VSA)

################################ R Code 12.64 ###################################

predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi
}

################################ R Code 12.65 ###################################

val.errors <- rep(NA, 6) 
Y <- HSWRESTLER[test, ]$hwfat  # hwfat values from test
for(i in 1:6){
  Yhat <- predict(model.exh, newdata = HSWRESTLER[test, ], id = i)
  val.errors[i] <- mean((Y - Yhat)^2)
}
val.errors
coef(model.exh, which.min(val.errors))
regfit.best <- regsubsets(hwfat ~ ., data = HSWRESTLER[-study, 1:7])
coef(regfit.best, which.min(val.errors))

################################ R Code 12.66 ###################################

HSW <- HSWRESTLER[-study, 1:7] 
n <- nrow(HSW)
k <- n                  # set the number of folds equal to n
set.seed(5)             # set for reproducible results
folds <- sample(x = 1:k, size = nrow(HSW), replace = FALSE)
cv.errors <- matrix(NA, k, 6, dimnames = list(NULL, paste(1:6)))
for(j in 1:k){
  best.fit <- regsubsets(hwfat ~., data = HSW[folds != j, ])
  for(i in 1:6){
    pred <- predict(best.fit, newdata = HSW[folds ==j, ], id = i)
    cv.errors[j, i] <- mean((HSW$hwfat[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
coef(regfit.best, which.min(mean.cv.errors))

################################ R Code 12.67 ###################################

HSW <- HSWRESTLER[-study, 1:7] 
n <- nrow(HSW)
k <- 5                  # set the number of folds equal to 5
set.seed(5)             # set for reproducible results
folds <- sample(x = 1:k, size = nrow(HSW), replace = TRUE)
cv.errors <- matrix(NA, k, 6, dimnames = list(NULL, paste(1:6)))
for(j in 1:k){
  best.fit <- regsubsets(hwfat ~., data = HSW[folds != j, ])
  for(i in 1:6){
    pred <- predict(best.fit, newdata = HSW[folds ==j, ], id = i)
    cv.errors[j, i] <- mean((HSW$hwfat[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
coef(regfit.best, which.min(mean.cv.errors))

################################ R Code 12.68 ###################################

mod.5fcv <- lm(hwfat ~ abs + triceps, data = HSW)
summary(mod.5fcv)
qqPlot(mod.5fcv)
residualPlots(mod.5fcv)
influenceIndexPlot(mod.5fcv)
scatter3d(hwfat ~ abs + triceps, data = HSW, surface.alpha = 0.1,
         point.col = "lightblue", grid = TRUE)


################################ Figure 12.26 ###################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mar = c(2, 2, 2, 2))
x <- seq(0.5, 7, 0.01)
y <- log(x)
plot(x, log(x), type = "l", xlim = c(-5, 7), ylim = c(-.8, 2), axes = FALSE, xlab = "", ylab = "")
arrows(0, -.8, 0, 2, code = 3, length = 0.1)
arrows(-0.4, 0, 7, 0, code = 3, length = 0.1)
polygon(x = c(3, 3, 0, 0, 3.3, 3.3), 
        y = c(0, log(3), log(3), log(3.3), log(3.3), 0), col = "gray")
polygon(x = c(6, 6, 0, 0, 6.6, 6.6), 
        y = c(0, log(6), log(6), log(6.6), log(6.6), 0), col = "gray")
lines(x, log(x))
text(1, -.1, "1")
text(3, -.1, "3")
text(3.3, -.1, "3.3")
text(6, -.1, "6")
text(6.6, -.1, "6.6")
arrows(3, -.3, 3.15, -0.02, code = 2, length = .05)
arrows(6.15, -.3, 6.3, -0.02, code = 2, length= .05)
text(3, -.4, "$\\frac{\\Delta x}{x} = \\frac{0.3}{3}=0.1$")
text(6.15, -.4, "$\\frac{\\Delta x}{x} = \\frac{0.6}{6}=0.1$")
text(-3, log(6.3), "$\\Delta \\ln(x) \\approx \\Delta y \\approx 10\\%$")
arrows(-1.5, log(6.3), -0.25, log(6.3), code = 2, length = 0.05)
text(-3, log(3.15), "$\\Delta \\ln(x) \\approx \\Delta y \\approx 10\\%$")
arrows(-1.5, log(3.15), -0.25, log(3.15), code = 2, length = 0.05)
text(3.5, log(4.5), "$y = \\ln(x)$")
par(opar)

################################ R Code 12.69 ###################################

library(MASS)
SA <- Animals[order(Animals$body), ]  # sorted by body
NoDINO <- SA[-c(28:26), ]             # remove dinosaurs 
simple.model <- lm(log(brain) ~ log(body), data = NoDINO)
coef(simple.model)

################################ 12.14 Qualitative Predictors ###################
contr.treatment(4)

################################ Example 12.26 ###################################

contrasts(EPIDURAL$ease)
 
EPIDURAL$ease <- factor(EPIDURAL$ease, 
                        levels = c("Easy", "Difficult", "Impossible"))
levels(EPIDURAL$ease)
contrasts(EPIDURAL$ease)


################################ Figure 12.27 ###################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow=c(2, 2))
par(mar = c(4, 4, 4, 2))
par(las = 1)
x <- seq(0, 10, .1)
y <- x
plot(x, y, type = "n",  axes = FALSE, xlab = "", ylab = "", main = "I")
arrows(0, 0, 0, 10, code = 2, length = 0.1)
arrows(0, 0, 10, 0, code = 2, length = 0.1)
segments(1, 3, 9, 7)
mtext(text = "$x$", side = 1, line = 0, at = 5)
mtext(text = "$Y$", side = 2, line = 0, at = 5)
plot(x, y, type = "n",  axes = FALSE, xlab = "", ylab = "", main = "II")
arrows(0, 0, 0, 10, code = 2, length = 0.1)
arrows(0, 0, 10, 0, code = 2, length = 0.1)
segments(1, 3, 9, 5)
segments(1, 5, 9, 7)
mtext(text = "$x$", side = 1, line = 0, at = 5)
mtext(text = "$Y$", side = 2, line = 0, at = 5)
plot(x, y, type = "n",  axes = FALSE, xlab = "", ylab = "", main = "III")
arrows(0, 0, 0, 10, code = 2, length = 0.1)
arrows(0, 0, 10, 0, code = 2, length = 0.1)
segments(0, 3, 9, 5)
segments(0, 3, 9, 8)
mtext(text = "$x$", side = 1, line = 0, at = 5)
mtext(text = "$Y$", side = 2, line = 0, at = 5)
plot(x, y, type = "n",  axes = FALSE, xlab = "", ylab = "", main = "IV")
arrows(0, 0, 0, 10, code = 2, length = 0.1)
arrows(0, 0, 10, 0, code = 2, length = 0.1)
segments(1, 3, 9, 8)
segments(1, 4, 9, 5)
mtext(text = "$x$", side = 1, line = 0, at = 5)
mtext(text = "$Y$", side = 2, line = 0, at = 5)
par(opar)
 

################################ R Code 12.70 ###################################

VIT2005$elevator <- factor(VIT2005$elevator, labels = c("No", "Yes"))
contrasts(VIT2005$elevator)

################################ R Code 12.71 ###################################

mod.simple <- lm(totalprice ~ area, data = VIT2005)
summary(mod.simple)

################################ R Code 12.72 ###################################

################################ Figure 12.28 ###################################

p <- ggplot(data = VIT2005,
           aes(x = area, y = totalprice, color = elevator))
p + geom_point() +
 scale_color_grey(start = 0.2, end = 0.8) +
 theme_bw() +
 labs(x = "\nLiving Area in Square Meters",
      y = "Appraised Price in Euros\n") +
 geom_abline(intercept = coef(mod.simple)[1],
             slope = coef(mod.simple)[2], colour = "blue")

################################ R Code 12.73 ###################################

mod.total <- lm(totalprice ~ area + elevator + area:elevator, 
                data = VIT2005)
mod.simple <- lm(totalprice ~ area, data = VIT2005)
anova(mod.simple, mod.total)  # compare models

################################ R Code 12.74 ###################################

anova(mod.total)  # ANOVA

################################ R Code 12.75 ###################################

mod.total <- lm(totalprice ~ area + elevator + area:elevator, 
                data = VIT2005)
mod.inter <- lm(totalprice ~ area + area:elevator, data = VIT2005)
anova(mod.inter, mod.total)  # compare models
 
coef(summary(mod.inter))


################################ Figure 12.29 ###################################

p <- ggplot(data = VIT2005, aes(x = area, y = totalprice, color = elevator))
p + geom_point() + 
  theme_bw() + 
  scale_color_grey(start = .2, end = .8) + 
  labs(x = "\nLiving Area in Square Meters", y = "Appraised Price in Euros\n") +
  geom_abline(intercept = 71352.0844, slope = 1897.9368, colour = "black") + 
  geom_abline(intercept = 71352.0844, slope = 1897.9368 + 553.9856, colour = "gray")

################################ R Code 12.76 ###################################

mod.lm <- lm(gpa ~ sat, data = GRADES)
betahat <- coef(mod.lm)
betahat
 
Xh <- matrix(c(1, 1300), nrow = 1)
Yhath <- Xh%*%betahat
Yhath
 
predict(mod.lm, newdata=data.frame(sat = 1300))

################################ R Code 12.77 ###################################

MSE <- anova(mod.lm)[2, 3]
MSE
XTXI <- summary(mod.lm)$cov.unscaled
XTXI
var.cov.b <- MSE*XTXI
var.cov.b
s2yhath <- Xh%*%var.cov.b%*%t(Xh)
s2yhath
syhath <- sqrt(s2yhath)
syhath
crit.t <- qt(0.95, 198)
CI.EYh <- Yhath + c(-1, 1)*crit.t*syhath
CI.EYh
 
predict(mod.lm, newdata = data.frame(sat = 1300), 
        interval = "conf", level = 0.90)
 
PI <- predict(mod.lm, newdata = data.frame(sat = 1300), 
        interval = "pred", level = 0.90)
 
s2yhathnew <- MSE + s2yhath
syhathnew <- sqrt(s2yhathnew)
syhathnew
PI <- Yhath + c(-1, 1)*crit.t*syhathnew
PI
 
PI <- predict(mod.lm, newdata = data.frame(sat = 1300), 
        interval = "pred", level = 0.90)
PI
 

################################ R Code 12.78 ###################################

pred.frame <- data.frame(sat = seq(700, 1600, 5))
CE <- predict(mod.lm, interval = "conf", newdata = pred.frame, level = 0.90)
PE <- predict(mod.lm, interval = "pred", newdata = pred.frame, level = 0.90)
plot(gpa ~ sat, data = GRADES)
matlines(pred.frame$sat, CE, lty = c(1, 3, 3), col = "black")
matlines(pred.frame$sat, PE, lty = c(1, 2, 2), col = "black")
syhath <- predict(mod.lm, interval = "conf", level = 0.90, 
                  newdata = pred.frame, se = TRUE)$se.fit
Yhath <- CE[, "fit"]
CV <- sqrt(2*qf(0.95, 2, 198))
LL <- Yhath - CV*syhath
UL <- Yhath + CV*syhath
CB <- cbind(LL, UL)
matlines(pred.frame$sat, CB, lty = c(1, 1), col = "gray")

################################ R Code 12.79 ###################################

alpha <- 0.10
mult.model <- lm(hwfat ~ age + abs + triceps, data = HSWRESTLER)
coef(summary(mult.model))
b <- coef(summary(mult.model))[2:4, 1]
s.b <- coef(summary(mult.model))[2:4, 2]
g <- 3
B <- qt((1 - alpha/(2*g)), 78 - 4)
B
BonSimCI.b <- matrix(c(b - B*s.b, b + B*s.b), ncol=2)
conf <- c("5%","95%")
bnam <- c("age","abs","triceps")
dimnames(BonSimCI.b) <- list(bnam, conf)
BonSimCI.b  # Bonferroni simulataneous CIs

################################ R Code 12.80 ###################################

Q <- 3
S <- sqrt(Q*qf(0.9, Q, 78 - 4))
S
SchSimCI.b <- matrix(c(b - S*s.b, b + S*s.b), ncol = 2)
dimnames(SchSimCI.b) <- list(bnam, conf)
SchSimCI.b
 

################################ R Code 12.82 ###################################

################################ Figure 12.31 ###################################

confidenceEllipse(mult.model, level = 0.90, which.coef = c(3, 4), 
                  Scheffe = TRUE, main = "")
title(main="Scheffe Confidence Region")
abline(v=SchSimCI.b[2, ])
abline(h=SchSimCI.b[3, ])
 
################################ R Code 12.83 ###################################

g <- 3
alpha <- 0.10
SC <- sqrt(g*qf(1 - alpha,3, 74))
TC <- qt(1 - alpha/(2*g), 74)
RES <- predict(mult.model, newdata = data.frame(age = c(16, 17, 18), 
              abs = c(10, 11, 8), triceps =c(9, 11, 8)), se.fit = TRUE)
Yhath <- RES$fit
Syhath <- RES$se.fit
ll <- Yhath - TC*Syhath
ul <- Yhath + TC*Syhath
BCI <- cbind(Yhath, Syhath, ll, ul)
BCI

################################ R Code 12.84 ###################################
 
g <- 3
alpha <- 0.10
SC <- sqrt(g*qf(1 - alpha, 3, 74))
TC <- qt(1 - alpha/(2*g), 74)
c(SC, TC)
MSE <- anova(mult.model)[4, 3]
MSE
s2yhathnew <- MSE + Syhath^2
Syhathnew <- sqrt(s2yhathnew)
ll <- Yhath - TC*Syhathnew
ul <- Yhath + TC*Syhathnew
SPI <- cbind(Yhath, Syhathnew, ll, ul)
SPI

