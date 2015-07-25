################### Chapter 11 Script
################### 7/25/15 V.1
library(PASWR2)
library(car)
library(multcomp)
library(gridExtra)
library(plyr)
#################################### Chapter 11 #############################




############################## Example: Tires ############################

population <- rep(LETTERS[1:4], 6)
set.seed(4)
Treatment <- sample(population, size = 24, replace = FALSE)
DF <- data.frame(Run = 1:24, Treatment)
head(DF)    # Show first six rows of DF


############################## Figure 11.3 ###############################

with(data = TIRE, oneway.plots(stopdist, tire))
 

############################## R Code 11.1 ###############################

TreatmentMeans <- tapply(TIRE$stopdist, TIRE$tire, mean)
a <- length(TreatmentMeans)
N <- length(TIRE$stopdist)
n <- xtabs(~TIRE$tire)[1]          # note all have 6
names(n) <- NULL                   # remove name
df.treat <- a - 1                  # df for treatments
df.error <- N - a                  # df for error
GrandMean <- mean(TIRE$stopdist)   # Y_{dot dot}
SStreat <- n*sum((TreatmentMeans - GrandMean)^2)
SStotal <- sum((TIRE$stopdist - GrandMean)^2)
SSerror <- sum((TIRE$stopdist - rep(TreatmentMeans, each = n))^2)
MStreat <- SStreat/df.treat
MSerror <- SSerror/df.error
Fobs <- MStreat/MSerror
pvalue <- pf(Fobs, df.treat, df.error, lower = FALSE)
 
TreatmentMeans <- tapply(TIRE$stopdist, TIRE$tire, mean)
TreatmentMeans                     # Y_{i dot}
a <- length(TreatmentMeans)
N <- length(TIRE$stopdist)
xtabs(~TIRE$tire)
n <- xtabs(~TIRE$tire)[1]          # note all have 6
names(n) <- NULL                   # remove name
df.treat <- a - 1                  # df for treatments
df.error <- N - a                  # df for error
GrandMean <- mean(TIRE$stopdist)   # Y_{dot dot}
GrandMean
SStreat <- n*sum((TreatmentMeans - GrandMean)^2)
SStreat
SStotal <- sum((TIRE$stopdist - GrandMean)^2)
SStotal
SSerror <- sum((TIRE$stopdist - rep(TreatmentMeans, each = n))^2)
SSerror
MStreat <- SStreat/df.treat
MStreat
MSerror <- SSerror/df.error
MSerror
Fobs <- MStreat/MSerror
Fobs
pvalue <- pf(Fobs, df.treat, df.error, lower = FALSE)
pvalue

############################## R Code 11.2 ###############################

mod <- aov(stopdist ~ tire, data = TIRE)
summary(mod)         # Show ANOVA table for mod
 
model.tables(mod, type = "means")

############################## R Code 11.3 ###############################

HypMeans <- c(405, 390)
a <- length(HypMeans)  # Number of groups
n <- 6                 # Number in each group
N <- a*n               # Total number of expt. units
df.error <- N - a      # DOF for error
Sigma <- 10
alpha <- 0.05
Y <- rep(HypMeans, each = n)                      # Responses
Treat <- factor(rep(LETTERS[c(2, 1)], each = 6))  # Treatment factor
SStreat <- summary(aov(Y ~ Treat))[[1]][1, 2]     # SS for treatment
lambda <- SStreat/Sigma^2
lambda
Gamma <- sqrt(lambda)
Gamma
CritT <- qt(1 - alpha, df.error)
Power <- pt(CritT, df.error, ncp = Gamma, lower = FALSE)
Power

power.t.test(n = 6, delta = 15, sd = 10, alternative = "one.sided")


############################## Figure 11.4 ###############################

curve(dt(x, 10), -4, 8, axes = FALSE, ann = FALSE, n = 500)
curve(dt(x, 10, Gamma), -4, 8, add = TRUE, n = 500)
x <- seq(-4, qt(0.95, 10), length = 200)
y <- dt(x, 10, Gamma)
xs <- c(-4, x, qt(0.95, 10))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue1")
x <- seq(qt(0.95, 10), 8, length = 200)
y <- dt(x, 10, Gamma)
xs <- c(qt(0.95, 10), x, 8)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue4")
# Retrace now
curve(dt(x, 10), -4, 8, add = TRUE, n = 500, lwd = 2)
curve(dt(x, 10, Gamma), -4, 8, add = TRUE, n = 500, lwd = 2)
# Highlight x-axis
segments(-4, 0, 8, 0, lwd = 3)
#
segments(qt(0.95, 10), 0, qt(0.95, 10), dt(qt(0.95, 10), 10, Gamma), lwd = 2)
###
arrows(7, .20, 3, .1, length = .1)
mtext("$\\pow(\\gamma=2.5981)$", side = 3, line = -5.5, at = 7)
mtext("$t_{10}$", side = 3, line = 0, at = 0)
mtext("$t^*_{10;\\nct = 2.5981}$", side = 3, line = -3, at = 4.5)
### labels now 
axis(side = 1, at = c(-4:8), line = -0.4)
####
arrows(qt(0.95, 10), 0.35, qt(0.95, 10), 0, length = 0.1, lwd = 2)
text(qt(0.95, 10), 0.37, "$t_{0.95; 10}$")


############################## R Code 11.4 ###############################

alpha <- 0.05
n <- 6
HypMeans <- c(390, 405, 415, 410)   # Hypothesized means
a <- length(HypMeans)               # Number of groups
N <- a*n                            # Total number of expt. units 
df.error <- N - a                   # DOF error 
Sigma <- 20
Y <- rep(HypMeans, each = n)                    # Responses 
Treat <- factor(rep(LETTERS[1:4], each = 6))    # Treatment factor
SStreat <- summary(aov(Y ~ Treat))[[1]][1, 2]   # SS treatment
lambda <- SStreat/Sigma^2
lambda
CritF <- qf(1 - alpha, a - 1, N - a)
CritF
TheoPower <- pf(CritF, a - 1, N - a, lambda, lower = FALSE)
TheoPower


############################## Figure 11.5 ###############################

curve(df(x, 3, 20), 0, 12, axes = FALSE, ann = FALSE, n = 500)
curve(df(x, 3, 20, lambda), 0, 12, add = TRUE, n = 500)
x <- seq(qf(0.95, 3, 20), 12, length = 200)
y <- df(x, 3, 20, lambda)
xs <- c(qf(0.95, 3, 20), x)
ys <- c(0, y)
polygon(xs, ys, col = "lightblue")
# Retrace now
curve(df(x, 3, 20), 0, 12, add = TRUE, n = 500, lwd = 2)
curve(df(x, 3, 20, lambda), 0, 12, add = TRUE, n = 500, lwd = 2)
# Highlight x-axis
segments(0, 0, 12, 0, lwd = 3)
###
arrows(8, .30, 4, .05, length = .1)
arrows(qf(0.95, 3, 20), 0.25, qf(0.95, 3, 20), 0, length= 0.1)
mtext("$\\pow(\\lambda=5.25)$", side = 3, line = -6.7, at = 8)
mtext("$F^*_{3, 20;\\lambda = 5.25}$", side = 3, line = -10.5, at = 8)
mtext("$F_{3, 20}$", side = 3, line =-5, at = 1.8)
### labels now 
axis(side = 1, at = c(0:12), line = -0.4)
####
text(qf(0.95, 3, 20), 0.29, "$f_{0.95; 3, 20}$")

############################## R Code 11.5 ###############################

set.seed(10)
a <- 4            # Number of groups
n <- 6            # Number in each group
alpha <- 0.05     # Alpha level
N <- a*n          # Total numberof expt. units
CritF <- qf(1 - alpha, a - 1, N - a)   # Critical F value
mu1 <- 390; mu2 <- 405; mu3 <- 415; mu4 <- 410  # True means
sigma <- 20                    # Assumed sigma  
SIMS <- 10^4                   # Numer of simulations
FS <- numeric(SIMS)            # Storage for FS
for(i in 1:SIMS){
  y1 <- rnorm(n, mu1, sigma)   # Values from mu1, sigma
  y2 <- rnorm(n, mu2, sigma)   # Values from mu2, sigma
  y3 <- rnorm(n, mu3, sigma)   # Values from mu3, sigma
  y4 <- rnorm(n, mu4, sigma)   # Values from mu4, sigma
  Y <- c(y1, y2, y3, y4)       # Combined reponses  
  treat <- factor(rep(LETTERS[1:4], each = n))  # Treatment factor
  FS[i] <- summary(aov(Y ~ treat))[[1]][1, 4]   # F values
}
SimPower <- mean(FS > CritF)   # Simulated power
SimPower


############################## R Code 11.6 ###############################

############################## Figure 11.6 ###############################
 
DF <- data.frame(x = FS)
x.dens <- density(FS)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
p <- ggplot(data = DF) 
p + geom_density(aes(x=x, y=..density..), fill="skyblue1", alpha=0.2) + 
  stat_function(fun = df, args = list(3, 20), n = 500)  + 
  geom_area(data = subset(df.dens, x >= CritF & x <= 15), 
               aes(x = x, y = y), fill = "skyblue4", alpha = 0.6) + 
  labs(x = "", y = "") + theme_bw() + coord_cartesian(xlim = c(0, 12)) 
 

############################## R Code 11.7 ###############################

TheoPower
SimPower
PerDiff <- (abs(TheoPower - SimPower)/TheoPower)*100
PerDiff       # less than 1% different
values <- c(0.01, 0.05, 0.10, 0.20, 0.80, 0.90, 0.95, 0.99)
TQ <- qf(values, 3, 20, lambda)       # theoretical qunatile
SQ <- quantile(FS, probs = values)    # simulated quantile
PD <- (abs(TQ - SQ)/TQ)*100           # percent difference
round(rbind(TQ, SQ, PD), 5)  

############################## R Code 11.8 ###############################

alpha <- 0.05
n <- 6                               # Number per group
HypMeans <- c(390, 405, 415, 410)    # Hypothesized means
a <- length(HypMeans)                # Number of groups  
N <- a*n                             # Total number of expt. units  
df.error <- N - a                    # DOF error
Sigma <- 10                          # Assumed sigma
Y <- rep(HypMeans, each = n)         # Reponses
Treat <- factor(rep(LETTERS[1:4], each = 6))   # Treatment factor
SStreat <- summary(aov(Y ~ Treat))[[1]][1, 2]  # SS treatment
lambda <- SStreat/Sigma^2            # Non-centrality parameter
lambda
CritF <- qf(1 - alpha, a - 1, N - a) # Critical F value
CritF
TheoPower <- pf(CritF, a - 1, N - a, lambda, lower = FALSE)
TheoPower

power.anova.test(groups = a, n = n, between.var = var(HypMeans), 
                 within.var = 10^2)


############################## R Code 11.9 ###############################

Sigma <- 20                             # Assumed sigma
Power <- 0                              # Initialize Power to 0
npg <- 1                                # Initial number per group  
HypMeans <- c(390, 405, 415, 410)       # Hypothesized means
a <- length(HypMeans)                   # Number of groups  
while(Power < 0.80){
  npg <- npg + 1                        # Increment npg by one
  N <- a*npg                            # Total number of extp. units  
  alpha <- 0.05                         # Alpha level
  Y <- rep(HypMeans, each = npg)        # Responses
  treat <- factor(rep(LETTERS[1:a], each = npg))  # Treatment factor
  SStreat <- summary(aov(Y ~ treat))[[1]][1, 2]   # SS treatment
  lambda <- SStreat/Sigma^2             # Non-centrality parameter
  CritF <- qf(1 - alpha, a - 1, N - a)  # Critical F value  
  Power <- pf(CritF, a - 1, N - a, ncp = lambda, lower = FALSE)
}
c(npg, lambda, Power)  

############################## R Code 11.10 ###############################

power.anova.test(groups = a, between.var = var(HypMeans), 
                 within.var = 20^2, power = 0.80)
npg <- ceiling(power.anova.test(groups = a, 
                                between.var = var(HypMeans), 
                                within.var = 20^2, power = 0.80)$n)
npg

############################## R Code 11.11 ###############################

alpha <- 0.05
n1 <- 6; n2 <- 6; n3 <- 12; n4 <- 12  # Numbers per group
HypMeans <- c(390, 405, 415, 410)     # Hypothesized means
a <- length(HypMeans)                 # Number of groups
N <- n1 + n2 + n3 + n4                # Total number of expt. units
df.error <- N - a                     # DOF error
Sigma <- 14                           # Assumed sigma
Y <- rep(HypMeans, times = c(n1, n2, n3, n4))  # Responses
Treat <- factor(rep(LETTERS[1:4], times = c(n1, n2, n3, n4))) 
SStreat <- summary(aov(Y ~ Treat))[[1]][1, 2]  # SS treatment
lambda <- SStreat/Sigma^2             # Non-centrality parameter 
lambda
CritF <- qf(1 - alpha, a - 1, N - a)  # Critical F value
CritF
TheoPower <- pf(CritF, a - 1, N - a, lambda, lower = FALSE)
TheoPower


############################## Figure 11.7 ###############################

curve(df(x, 3, 32), 0, 15, axes = FALSE, ann = FALSE, n = 500)
curve(df(x, 3, 32, lambda), 0, 15, add = TRUE, n = 500)
x <- seq(qf(0.95, 3, 32), 15, length = 200)
y <- df(x, 3, 32, lambda)
xs <- c(qf(0.95, 3, 32), x)
ys <- c(0, y)
polygon(xs, ys, col = "lightblue")
# Retrace now
curve(df(x, 3, 32), 0, 15, add = TRUE, n = 500, lwd = 2)
curve(df(x, 3, 32, lambda), 0, 15, add = TRUE, n = 500, lwd = 2)
# Highlight x-axis
segments(0, 0, 15, 0, lwd = 3)
###
arrows(9, .30, 5, .05, length = .1)
arrows(qf(0.95, 3, 32), 0.25, qf(0.95, 3, 32), 0, length= 0.1)
mtext("$\\pow(\\lambda=13.3929)$", side = 3, line = -6.5, at = 9)
mtext("$F^*_{3, 32;\\lambda = 13.3929}$", side = 3, line = -10, at = 11)
mtext("$F_{3, 32}$", side = 3, line =-3, at = 2)
### labels now 
axis(side = 1, line = -0.4)
####
text(qf(0.95, 3, 32), 0.30, "$f_{0.95; 3, 32}$")


############################## R Code 11.12 ###############################

############################## Figure 11.8  ###############################

mod.aov <- aov(stopdist ~ tire, data = TIRE)
r <- rstandard(mod.aov)
DF <- data.frame(TIRE, r = r)
head(DF)
p <- ggplot(data = DF, aes(x = order, y = r)) +
 geom_point() +
 geom_line() +
 geom_hline(yintercept = 0, linetype = "dashed") +
 labs(x = "Ordered Values", y = "Standardized Residuals") +
 theme_bw()
p

############################## R Code 11.13 ###############################

############################## Figure 11.9 ###############################

p <- ggplot(data = DF, aes(sample = r)) + 
  stat_qq() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  theme_bw()
p

############################## R Code 11.14 ###############################

shapiro.test(r)

############################## Figure 11.10 ###############################

mod.aov <- aov(stopdist ~ tire, data = TIRE)
r <- rstandard(mod.aov)  # standardized residuals
fv <- fitted(mod.aov)    # fitted values
DF <- data.frame(x = fv, y = r)
p <- ggplot(data = DF, aes(x = x, y = y)) +
  geom_point() +
  labs(x = "Fitted Values", y = "Standardized Residuals") + 
  theme_bw()
p

############################## R Code 11.15 ###############################

med <- tapply(TIRE$stopdist, TIRE$tire, median)
Zij <- abs(TIRE$stopdist - med[TIRE$tire])
TIREA <- data.frame(TIRE, Zij)
summary(aov(Zij ~ tire, data = TIREA))
# Or using the leveneTest()
car::leveneTest(mod.aov)

############################## R Code 11.16 ###############################

TIREO <- TIRE[order(TIRE$order),]  # reordering the DF
head(TIREO)
mod.aovO <- aov(stopdist ~ tire, data = TIREO)

############################## Figure 11.11 ###############################

checking.plots(mod.aovO) 

############################## Figure 11.12 ###############################

xs <- seq(0.0001, 10, length = 100)
lamN1 <- xs^(-1)
lam0 <- log(xs)
lam0.5 <- xs^(0.5)
lam1 <- xs^1
lam2 <- xs^2
XS <- rep(xs, 5)
length(XS)
DF1 <- stack(list("$\\lambda = -1$" = lamN1, "$\\lambda = 0$" = lam0,
                   "$\\lambda = 0.5$" = lam0.5, "$\\lambda = 1$" = lam1,
                   "$\\lambda = 2$" = lam2))
DF4 <- cbind(XS, DF1)
head(DF4)
ggplot(data = DF4, aes(x = XS, y = values, group = ind, linetype = ind, color = ind)) + geom_line(size = 1) + ylim(-2, 10) + theme_bw() + 
  labs(linetype = "Transformation", color = "Transformation", x = "Original Values", y = "Transformed Values")


############################## Figure 11.13 ###############################

FCD.aov <- aov(weight ~ diet, data = FCD)
checking.plots(FCD.aov) # recreate this one

############################## Figure 11.14 ###############################

FCDlog.aov <- aov(log(weight) ~ diet, data = FCD)
checking.plots(FCDlog.aov) # recreate this one as well

############################## R Code 11.17 ###############################

med <- tapply(FCD$weight, FCD$diet, median)
Zij <- abs(FCD$weight - med[FCD$diet])
FCDA <- cbind(FCD, Zij)
ANOVAa <- summary(aov(Zij ~ diet, data = FCDA))
ANOVAa
medL <- tapply(log(FCD$weight), FCD$diet, median)
ZijL <- abs(log(FCD$weight) - log(med[FCD$diet]))
FCDAL <- cbind(FCD, ZijL)
ANOVAb <- summary(aov(ZijL ~ diet, data = FCDAL))
ANOVAb

############################## R Code 11.18 ###############################

ni <- with(data = FCD, tapply(weight, diet, length)) # Number per group
ni
a <- length(ni)
si2 <- with(data = FCD, tapply(weight, diet, var))   # Variance per group
si2
ybar <- with(data = FCD, tapply(weight, diet, mean)) # Mean per group
ybar
wi <- ni/si2
ytild <- sum(wi*ybar)/sum(wi)
ytild
wlamb <- 3*sum((1 - (wi/sum(wi)))^2/(ni - 1))/(a^2 - 1)
wlamb
dfn <- (a - 1)            # Degrees of freedom numerator
dfn
dfd <- 1/wlamb            # Degrees of freedom denominator
dfd
W <- sum(wi*(ybar - ytild)^2/(a - 1))/(1 + 2/3*(a - 2)*wlamb)
W                         # Computed W value 
pvalue <- pf(W, dfn, dfd, lower = FALSE)
pvalue
# Or
oneway.test(weight ~ diet, data = FCD)

############################## 11.7.2 The Tukey's HSD ###############################

qtukey(0.95, 4, 20)

############################## R Code 11.19 ###############################

mod.dro <- aov(fecundity ~ line, data = DROSOPHILA)
summary(mod.dro)  # ANOVA

############################## R Code 11.20 ###############################

MSerror <- summary(mod.dro)[[1]][2, 3]
SStreat <- summary(mod.dro)[[1]][1, 2]
DFerror <- summary(mod.dro)[[1]][2, 1]
ybar <- with(data = DROSOPHILA, tapply(fecundity, line, mean))
ni <- xtabs(~ line, data = DROSOPHILA)
ci <- c(1, -0.5, -0.5)
di <- c(0, 1, -1)
ortho <- sum(ci*di/ni)  # verify orthogonality
ortho
SSC1 <- (sum(ci*ybar))^2/sum(ci^2/ni)
SSC2 <- (sum(di*ybar))^2/sum(di^2/ni)
FC1 <- SSC1/MSerror
FC2 <- SSC2/MSerror
Fs <- c(FC1, FC2)
pvalue <- pf(Fs, 1, DFerror, lower = FALSE)
res <- cbind(SS = c(SSC1, SSC2), Fs, pvalue)
rownames(res) <- c("C1", "C2")
res
  
contrasts(DROSOPHILA$line)
 
contrasts(DROSOPHILA$line) <- contr.helmert(levels(DROSOPHILA$line))
contrasts(DROSOPHILA$line)

############################## R Code 11.21 ###############################

contrasts(DROSOPHILA$line)[, 1] <- ci
contrasts(DROSOPHILA$line)[, 2] <- di
CON <- contrasts(DROSOPHILA$line)
colnames(CON) <- c("Non vs. Res and Sus", "Res vs. Sus")
CON
summary(aov(fecundity ~ C(line, CON, 1) + C(line, CON, 2), 
            data = DROSOPHILA))

############################## R Code 11.22 ###############################

summary(lm(fecundity ~ line, data = DROSOPHILA)) # lm output
 

############################## R Code 11.23 ###############################

library(multcomp)                # needed for glht()
MC <- glht(model = mod.dro, linfct = mcp(line = t(CON)) )
summary(MC)                      # Summary of MC object
CI <- confint(MC, level = 0.95)  # 95% CIs for contrasts
CI


############################## R Code 11.24 ###############################

############################## Figure 11.15 ###############################

opar <- par(no.readonly = TRUE)     # Read in graphical parameters
par(mar = c(5.1, 10.1, 4.1, 2.1))   # Enlarge left margin
CI <- confint(MC, level = 0.95)     # Compute 95% CIs
plot(CI)                            # Graph CIs
par(opar)                           # Reset graphical parameters
 
############################## Example 11.5 ###############################

pairwise.t.test(TIRE$stopdist, TIRE$tire, p.adjust.method = "none")
 
pairwise.t.test(TIRE$stopdist, TIRE$tire, p.adjust.method = "bonferroni")
 

############################## R Code 11.25 ###############################

mod.tire <- aov(stopdist ~ tire, data = TIRE)
CI2 <- TukeyHSD(mod.tire, which = "tire")
CI2

############################## R Code 11.26 ###############################

############################## Figure 11.16 ###############################

tire.hsd <- data.frame(CI2$tire)
tire.hsd$Comparison <- row.names(tire.hsd)
ggplot(data = tire.hsd, aes(x = Comparison, y = diff, ymin = lwr,
                           ymax = upr)) +
 geom_pointrange() +
 geom_hline(yintercept = 0, linetype = "dashed") +
 geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
 labs(y = "\nDifferences in mean levels of tire", x = "",
      title = "95% family-wise confidence level\n") +
 coord_flip() +
 theme_bw()

############################## R Code 11.27 ###############################

alpha <- 0.05
mod.tire <- aov(stopdist ~ tire, data = TIRE)
a <- length(levels(TIRE$tire))
N <- length(TIRE$stopdist)
dfe <- N - a
MSE <- summary(mod.tire)[[1]][2, 3]
MEANS <- tapply(TIRE$stopdist, TIRE$tire, mean)
MM <- outer(MEANS, MEANS, "-")
ni <- xtabs(~tire, data = TIRE)[1]   # ni's are all the same (6)
names(ni) <- NULL
K <- choose(a, 2)                    # number of comparisons (6)
tLSD <- qt(1 - alpha/2, dfe)         # LSD critical value
tBSD <- qt(1 - alpha/(2*K), dfe)     # Bonferroni critical value   
tHSD <- qtukey(1 - alpha, a, dfe)    # Tukey critical value
cSCH <- sqrt((a - 1)*qf(1 - alpha, a - 1, dfe))  # Scheffe CV
LSD <- tLSD * sqrt(MSE) * sqrt(1/ni + 1/ni)
BON <- tBSD * sqrt(MSE) * sqrt(1/ni + 1/ni)
HSD <- tHSD * sqrt(MSE) * sqrt(1/ni) 
# Note: Contrast ci's are (1, -1, 0, 0) ... so sum(ci^2) = 2
SCH <- cSCH * sqrt(MSE * 2/ni)
c(LSD = LSD, BON = BON, HSD = HSD, SCH = SCH)  # Sig differences
MM                                   # outer(MEANS, MEANS, "-")
MM[lower.tri(MM)]
 
 
############################## R Code 11.28 ###############################

carrot.mod <- aov(shear ~ freezer, data = FOOD)
n <- xtabs(~freezer, data = FOOD)[1]  # Number per freezer
names(n) <- NULL
ANOVA <- summary(carrot.mod)  # ANOVA
ANOVA
MST <- ANOVA[[1]][1, 3]               # MS treatments
MSE <- ANOVA[[1]][2, 3]               # MS error
sig2tau <- (MST - MSE)/n
sig2tau

############################## R Code 11.29 ###############################

car <- rep(c("Car1", "Car2", "Car3", "Car4"), each = 4)
tire <- rep(LETTERS[1:4], each = 4)
set.seed(13)
CRD <- sample(tire)
CRBD <- c(sample(LETTERS[1:4]), sample(LETTERS[1:4]), 
          sample(LETTERS[1:4]), sample(LETTERS[1:4]))
DDF <- cbind(car, tire, CRD, CRBD)
DDF
 

############################## R Code 11.30 ###############################

############################## Figure 11.17 ###############################
 
ggplot(data = TIREWEAR, aes(x = treat, y = wear, shape = block, 
                            group = block, linetype = block)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  labs(y = "Mean Tire Wear\n(thousandths of an inch)", x = "\nTire Type") +
  theme_bw()

ggplot(data = TIREWEAR, aes(x = block, y = wear, shape = treat, 
                            group = treat, linetype = treat)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  labs(y = "Mean Tire Wear\n(thousandths of an inch)", x = "\nblock") +
  theme_bw() 

############################## R Code 11.31 ###############################

############################## Figure 11.18 ###############################

p1 <- ggplot(data = TIREWEAR, aes(x = wear, y = treat, shape = treat)) +
  geom_point(size = 3) + 
  facet_grid(. ~ block) + 
  theme_bw()
p2 <- ggplot(data = TIREWEAR, aes(x = wear, y = block, shape = block)) +
  geom_point(size = 3) + 
  facet_grid(. ~ treat) + 
  theme_bw()
library(gridExtra)
grid.arrange(p1 + guides(color = FALSE), p2 + guides(color = FALSE), 
             ncol = 1)

############################## Figure 11.19 ###############################

with(data = TIREWEAR,
     plot.design(wear ~ treat + block))
 
mod.aov <- aov(wear ~ treat + block, data = TIREWEAR)
summary(mod.aov) # ANOVA

############################## R Code 11.32 ###############################

yidotbar <- tapply(TIREWEAR$wear, TIREWEAR$treat, mean)
ydotjbar <- tapply(TIREWEAR$wear, TIREWEAR$block, mean)
ydotdotbar <- mean(TIREWEAR$wear)
tauihat <- yidotbar - ydotdotbar
betajhat <- ydotjbar - ydotdotbar
eijhat <- resid(mod.aov)
muhatmat <- matrix(rep(ydotdotbar, 16), nrow = 4)
muhatmat
treatmat <- matrix(rep(tauihat, 4), nrow = 4, byrow = FALSE)
treatmat
blockmat <- matrix(rep(betajhat, 4), nrow = 4, byrow = TRUE)
blockmat
residmat <- matrix(eijhat, nrow = 4, byrow = FALSE)
residmat
yijmat <- muhatmat + treatmat + blockmat + residmat
yijmat

############################## R Code 11.33 ###############################

ORD <- c(4, 1, 3, 2, 5, 6, 7, 8, 9, 10, 12, 11, 15, 13, 16, 14)
TIREWEARO <- TIREWEAR[order(ORD),]
head(TIREWEARO)
mod.aovO <- aov(wear ~ treat + block, data = TIREWEARO)


############################## Figure 11.20 ###############################

checking.plots(mod.aovO) 


############################## R Code 11.34 ###############################

CI <- TukeyHSD(mod.aov, which = "treat")
CI


############################## Figure 11.21 ###############################

tire.hsd <- data.frame(CI$treat)
tire.hsd$Comparison <- row.names(tire.hsd)
ggplot(data = tire.hsd, aes(x = Comparison, y = diff, ymin = lwr, ymax = upr)) + 
  geom_pointrange(size = 0.8) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, size = 0.8) + 
  labs(y = "\nDifferences in mean tread loss of tire", x = "", 
       title = "95\\% family-wise confidence level\n") +
  coord_flip() + 
  theme_bw(base_size = 16) 
 

############################## Figure 11.22 ###############################

library(plyr)
MEANS <- tapply(TIREWEAR$wear, TIREWEAR$treat, mean)
mdf <- ddply(TIREWEAR, "treat", summarize, mwear = mean(wear))
MSE <- summary(mod.aov)[[1]][3, 3]
me <- qt(0.975, 9)*MSE^.5/sqrt(4)
ggplot(data = mdf, aes(x = treat, y = mwear))  + 
  geom_bar(stat = "identity", fill = "pink", alpha = 0.6) + 
  theme_bw() + 
  geom_errorbar(aes(ymin = mwear - me, ymax = mwear + me), width = 0.3) + 
  labs(x = "", y = "Mean Wear (thousandths of an inch)\n")


############################## R Code 11.35 ###############################

Microamps <- c( 280, 290, 285, 300, 310, 295, 270, 285, 290, 230, 
                235, 240, 260, 240, 235, 220, 225, 230)
Glass <- factor(c(rep("Glass I", 9), rep("Glass II", 9)))
Phosphor <- factor(rep(c(rep("Phosphor A", 3), 
                             rep("Phosphor B", 3),
                             rep("Phosphor C", 3)), 2))
DF <- data.frame(Microamps, Glass, Phosphor)
rm(Microamps ,Glass, Phosphor)  # Clean up workspace
 

############################## Figure 11.23 ###############################

with(data = DF,
    twoway.plots(Microamps, Glass, Phosphor)
    )

############################## R Code 11.36 ###############################

############################## Figure 11.24 ###############################
 
p1 <- ggplot(data = DF, aes(x = Glass, y = Microamps,
                           colour = Phosphor, group = Phosphor,
                           linetype = Phosphor)) +
 stat_summary(fun.y = mean, geom = "point") +
 stat_summary(fun.y = mean, geom = "line") +
 theme_bw()
p2 <- ggplot(data = DF, aes(x = Phosphor, y = Microamps, colour = Glass,
                           group = Glass, linetype = Glass)) +
 stat_summary(fun.y = mean, geom = "point") +
 stat_summary(fun.y = mean, geom = "line") +
 theme_bw()
library(gridExtra)
grid.arrange(p1, p2, nrow = 2)


############################## R Code 11.37 ###############################

############################## Figure 11.25 ###############################

library(plyr)                          # loaded for function ddply
mdf <- ddply(DF, c("Glass", "Phosphor"), summarize,
            mmicro = mean(Microamps))
mdf   #  Mean micoramps for 6 combinations as data frame.
p <- ggplot(data = mdf, aes(x = Phosphor, y = mmicro, fill = Glass))
p + geom_bar(position = "dodge", stat = "identity") +
 scale_fill_grey(start = 0.4, end = 0.8) +
 labs(x = "") +
 labs(x = "", y = "Microamps") +
 theme_bw()
 

############################## R Code 11.38 ###############################

mod1.TVB <- aov(Microamps ~ Glass + Phosphor + Glass:Phosphor, data = DF)
model.tables(mod1.TVB, type = "means")
model.tables(mod1.TVB, type = "effects")

############################## R Code 11.39 ###############################
anova(mod1.TVB)


############################## Figure 11.26 ###############################
checking.plots(mod1.TVB) 
 

############################## R Code 11.40 ###############################

library(car)
leveneTest(Microamps ~ Glass*Phosphor, data = DF)

############################## R Code 11.41 ###############################

mod1.TVB <- aov(Microamps ~ Glass * Phosphor, data = DF)
CIs <-  TukeyHSD(mod1.TVB, which = c("Phosphor", "Glass"))
CIs

############################## R Code 11.42 ###############################

opar <- par(no.readonly = TRUE)
par(mar=c(4.1, 10.1, 5.1, 2.1), cex.axis = 0.8)
plot(TukeyHSD(mod1.TVB, which = c("Glass")), las = 1)
plot(TukeyHSD(mod1.TVB, which = c("Phosphor")), las = 1)
par(opar)

