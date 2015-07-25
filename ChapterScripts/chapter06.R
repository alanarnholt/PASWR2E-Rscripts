################### Chapter 06 Script
################### 7/24/15 V.1
library(PASWR2)
library(MASS)
############################# Chapter 6 ##########################


############################ Example 6.1 #########################

combn(x = 1:5, m = 3)

############################ R Code 6.1 #########################

t(srs(popvalues = c(2, 5, 8, 12, 13), n = 3))

############################ R Code 6.2 #########################

set.seed(13)                            # done for reproducibility
sample(x = 1:180, size = 5, replace=FALSE, prob = rep(1/180, 180))

############################ R Code 6.3 #########################

set.seed(13)  # done for reproducibility
sample(x = 1:20, size = 5, prob = c(rep(1/26, 18), rep(4/26, 2)), 
       replace = FALSE)

############################ R Code 6.4 #########################

set.seed(13)  # done for reproducibility
seq(sample(1:100,1), 1000, 100)

############################ R Code 6.5 #########################

set.seed(13)  # done for reproducibility
seq(sample(1:50, 1), 1000, 50)

############################ R Code 6.6 #########################

N <- 6
n <- 2
pop <- 1:N
rs <- expand.grid(Draw1 = pop, Draw2 = pop) # Possible random samples
xbarN <- apply(rs, 1, mean)                 # Means of all rs values
s2N <- apply(rs, 1, var)                    # Variance of all rs values
RSV <- cbind(rs, xbarN = xbarN, s2N = s2N)
head(RSV, n = 1)  # First 1 row of values for Case 1 (random sampling)

############################ R Code 6.7 #########################

library(MASS)                # used for function fractions()
fractions(xtabs(~xbarN)/36)  # Sampling dist of xbar (random sampling)
fractions(xtabs(~s2N)/36)    # Sampling dist of S2 (random sampling)

############################ R Code 6.8 #########################

T1 <- fractions(xtabs(~xbarN)/36)
T2 <- fractions(xtabs(~s2N)/36) 
XBAR <- as.numeric(names(T1))             # Unique values of xbar
S2 <- as.numeric(names(T2))               # Unique values of s2 
MU_xbarN <- sum(XBAR*T1)                  # Expected value of xbarN
MU_xbarN
VAR_xbarN <- sum((XBAR - MU_xbarN)^2*T1)  # Var xbarN
VAR_xbarN
MU_s2N <- sum(S2*T2)                      # Expected value of s2N
MU_s2N

############################ R Code 6.9 #########################

draw <- c("Draw1", "Draw2")
SRS <- srs(1:N, n)  # possible simple random samples
dimnames(SRS) <- list(NULL, draw)
xbarn <- apply(SRS, 1, mean)                 # Means of all SRS values
s2n <- apply(SRS, 1, var)                    # Variance of all SRS values
SRSV <- cbind(SRS, xbarn = xbarn, s2n = s2n)
head(SRSV, n = 3)  # First 3 rows of values for Case 2 (SRS)

############################ R Code 6.10 #########################

library(MASS)                # used for function fractions()
fractions(xtabs(~xbarn)/15)  # Sampling dist of xbar (SRS)
fractions(xtabs(~s2n)/15)    # Sampling dist of S2 (SRS)

############################ R Code 6.11 #########################

T3 <- fractions(xtabs(~xbarn)/15)
T4 <- fractions(xtabs(~s2n)/15) 
XBAR <- as.numeric(names(T3))             # Unique values of xbar
S2 <- as.numeric(names(T4))               # Unique values of s2 
MU_xbarn <- sum(XBAR*T3)                  # Expected value of xbarn
MU_xbarn
VAR_xbarn <- sum((XBAR - MU_xbarn)^2*T3)  # Var xbarn
VAR_xbarn
MU_s2n <- sum(S2*T4)                      # Expected value of s2n
MU_s2n

############################ Figure 6.1 #########################

T1 <- fractions(xtabs(~xbarN)/36)
T2 <- fractions(xtabs(~s2N)/36) 
T3 <- fractions(xtabs(~xbarn)/15)
T4 <- fractions(xtabs(~s2n)/15) 
XBARN <- as.numeric(names(T1))           # Unique values of xbar
S2N <- as.numeric(names(T2))             # Unique values of s2 
XBARn <- as.numeric(names(T3))           # Unique values of xbar
S2n <- as.numeric(names(T4))             # Unique values of s2 
DF <- data.frame(fx =c(T1, T2, T3, T4), x =c(XBARN, S2N, XBARn, S2n), stat=0, samt=0)
DF
DF$stat[1:11] = "$\\bar{X}$"
DF$stat[12:17] = "$S^2$"
DF$stat[18:26] = "$\\bar{X}$"
DF$stat[27:31] = "$S^2$"
DF$samt[1:17] = "Random Sample"
DF$samt[18:31] = "Simple Random Sample"
DF
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) + facet_grid(samt~stat) + 
  geom_linerange(aes(x = x, ymin = 0, ymax = fx), size = 1) + 
  geom_point(color = "blue", size = 2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "", x = "", title = "")
theme_set(previous_theme) # Restore original theme


############################ Figure 6.2 #########################

previous_theme <- theme_set(theme_bw()) # set black and white theme
DF1 <- data.frame(x = c(-2,0, 0,10,10, 12), y = c(0, 0, 0.1,0.1,0, 0))
p <- ggplot(data = DF1, aes(x = x, y = y))
p1 <- p + geom_area(fill = "skyblue3", alpha = 1) + ylim(0, .21) + 
  labs(x = "", y = "") 
p1
DF2 <- data.frame(x = c(-2, 0, 5, 10, 12), y = c(0, 0, .2, 0, 0))
p <- ggplot(data = DF2, aes(x = x, y = y))
p2 <- p + geom_area(fill = "skyblue3", alpha = 1) + ylim(0, .21) + 
  labs(x = "", y = "")
p2
p <- ggplot(data.frame(x = c(-2, 12)), aes(x = x)) + 
  stat_function(fun = dnorm, arg = list(mean= 5, sd = 2.0412), 
                geom = "area", fill = "blue") + ylim(0, .21)
p3 <- p + geom_area(aes(x = x, y = y), data =DF2, fill = "skyblue3", alpha = .7) + 
  ylim(0, .21) + labs(x = "", y = "")
p3
previous_theme <- theme_set(theme_bw()) # set black and white theme

############################ R Code 6.12 #########################

uv <- (10 + sqrt(300))/2  # b of X ~ Unif(a, b)
lv <- (10 - sqrt(300))/2  # a of X ~ Unif(a, b)
c(lv, uv)
m <- 100000
set.seed(15)
MEANSunif2 <- numeric(m) # storage space for means
for (i in 1:m) {MEANSunif2[i] <- mean(runif(2, lv, uv))}
MEANSexp2 <- numeric(m) # storage space for means
for (i in 1:m) {MEANSexp2[i] <- mean(rexp(2, 1/5))}
MEANSunif16 <- numeric(m) # storage space for means
for (i in 1:m) {MEANSunif16[i] <- mean(runif(16, lv, uv))}
MEANSexp16 <- numeric(m) # storage space for means
for (i in 1:m) {MEANSexp16[i] <- mean(rexp(16, 1/5))}
MEANSunif36 <- numeric(m) # storage space for means
for (i in 1:m) {MEANSunif36[i] <- mean(runif(36, lv, uv))}
MEANSexp36 <- numeric(m) # storage space for means
for (i in 1:m) {MEANSexp36[i] <- mean(rexp(36, 1/5))}
MEANSunif100 <- numeric(m) # storage space for means
for (i in 1:m) {MEANSunif100[i] <- mean(runif(100, lv, uv))}
MEANSexp100 <- numeric(m) # storage space for means
for (i in 1:m) {MEANSexp100[i] <- mean(rexp(100, 1/5))}
BDF <- data.frame(Means = c(MEANSunif2, MEANSexp2, MEANSunif16, 
                            MEANSexp16, MEANSunif36, MEANSexp36, 
                            MEANSunif100, MEANSexp100), 
                  Type =  rep( rep(c("Uniform", "Exponential"), 
                                   each = m), 4),
                  Size = c(rep("n = 2", 2*m), rep("n = 16", 2*m), 
                          rep("n = 36", 2*m), rep("n = 100", 2*m )) )
BDF$Size <- factor(BDF$Size, 
                   levels = c("n = 2", "n = 16", "n = 36", "n = 100"))
BDF$Type <- factor(BDF$Type, levels = c("Uniform", "Exponential"))

############################ R Code 6.13 #########################

############################ Figure 6.3  #########################

p <- ggplot(data = subset(BDF, Size == "n = 2"), aes(x = Means)) +
 geom_density(fill = "skyblue") +
 facet_grid(Type ~ Size) +
 coord_cartesian(xlim = c(lv, uv)) +
 labs(x = expression(bar(x))) +
 theme_bw()
p + stat_function(fun = dnorm, args = list(5, 5/sqrt(2)), col ="blue",
                 size = 1, fill = "blue", alpha = 0.2, geom = "area")


p <- ggplot(data = subset(BDF, Size == "n = 16"), aes(x = Means)) +
 geom_density(fill = "skyblue") +
 facet_grid(Type ~ Size) +
 coord_cartesian(xlim = c(0, 10)) +
 labs(x = expression(bar(x))) +
 theme_bw()
p + stat_function(fun = dnorm, args = list(5, 5/sqrt(16)), col ="blue",
                 size = 1, fill = "blue", alpha = 0.2, geom = "area")


############################ Figure 6.4  #########################

 p <- ggplot(data = subset(BDF, Size == "n = 36"), aes(x = Means)) + 
  geom_density(fill = "skyblue") + 
  facet_grid(Type ~ Size) +  
  coord_cartesian(xlim = c(2, 8)) +
  labs(x = expression(bar(x))) +
  theme_bw() 
p + stat_function(fun = dnorm, args = list(5, 5/sqrt(36)), col ="blue", 
                  size = 1, fill = "blue", alpha = 0.2, geom = "area")

p <- ggplot(data = subset(BDF, Size =="n = 100"), aes(x = Means)) + geom_density(fill = "skyblue") + 
  facet_grid(Type ~ Size) + theme_bw() + 
  coord_cartesian(xlim = c(3, 7)) +
  labs(x = expression(bar(x))) +
  theme_bw()
p + stat_function(fun = dnorm, args = list(5, 5/sqrt(100)), col ="blue", 
                  size = 1, fill = "blue", alpha = 0.2, geom = "area")
  


############################ Example 6.12  #########################

round(1 - pnorm(4.5, 4, sqrt(.03)), 3)
 

############################ R Code 6.14  #########################

set.seed(6)
m <- 20000; nx <- 100; ny <- 81; mux <- 100; sigx <- 10 
muy <- 50; sigy <- 9; muxy <- mux - muy
sigxy <- sqrt( (sigx^2/nx) + (sigy^2/ny) )
meansX <- numeric(m)  # vector of zeros
meansY <- numeric(m)  # vector of zeros
for(i in 1:m){meansX[i] <- mean(rnorm(nx, mux, sigx))}
for(i in 1:m){meansY[i] <- mean(rnorm(ny, muy, sigy))}
XY <- meansX - meansY
c(mean(XY), sd(XY))
mean(XY < 52)            # Simulation answer to P(XY < 52)
pnorm(52, 50, sqrt(2))   # Theoretical answer to P(XY < 52) 


############################ R Code 6.15  #########################

############################ Figure 6.5  #########################

p <- ggplot(data = data.frame(x = XY), aes(x = x))
p + geom_histogram(aes(y = ..density..), fill = "pink",
                  color = "black", binwidth = 0.5) +
 stat_function(fun = dnorm, args = list(50, sqrt(2)), size = 1) +
 labs(x = expression(bar(X) - bar(Y)), y = "") +
 theme_bw()
  

############################ Example  6.15  #########################

1 - pnorm(0.11, 0.10, sqrt(0.1*0.9/500))
1 - pnorm(55, 500*0.1, sqrt(500*0.1*0.9))
1 - pbinom(54, 500, 0.10)
pnorm(0.12, 0.10, sqrt(0.1*0.9/500))
pnorm(60, 500*.10, sqrt(500*0.1*0.9))
pbinom(60, 500, 0.1)
 

############################ Example  6.16  #########################
 
sig <- sqrt((0.383*0.617)/250)
pnorm(0.402, 0.383, sig) - pnorm(0.358, 0.383, sig)



pbinom(100, 250, 0.383) - pbinom(89, 250, 0.383)

############################ 6.6.1 Chi-Square Distribution #######################

qchisq(.95, 10)

############################ Figure  6.6  #########################

curve(dchisq(x, 3), 0, 35, axes = FALSE, ann = FALSE, n = 500, lwd = 2)
curve(dchisq(x, 6), 0, 35, add = TRUE, n = 500, lty = "dashed", lwd = 2)
curve(dchisq(x, 16), 0, 35, add = TRUE, n = 500, lty = "dotted", lwd = 2)
axis(side = 1)
text(5.5, 0.20, "n = 3")
text(10, 0.1, "n = 6")
text(24, 0.05, "n = 16")
segments(0, 0, 35, 0, lwd = 2)

############################ Example  6.17  #########################

pnorm(sqrt(2*126) -sqrt(299), lower = FALSE)
1 - pchisq(126, 150)
 
pnorm(sqrt(100) - sqrt(129)) - pnorm(sqrt(80)-sqrt(129))
pchisq(50, 65) - pchisq(40, 65)
 
pnorm(sqrt(2*260) - sqrt(2*220 -1), lower = FALSE)
pchisq(260, 220, lower = FALSE)
 
(qnorm(0.60) + sqrt(199))^2/2
qchisq(0.6, 100)

############################ Example  6.18  #########################
 
pchisq(24, 10)
 
pchisq(12.175*10/25, 10, lower = FALSE)
 
qchisq(0.50, 10)
 
a <- sqrt(qchisq(0.50, 10)*25/10)
a
 
############################ Example  6.20  #########################

pchisq(15.98712, 10) - pchisq(4.865182, 10)
 
pchisq(30*(18.11898^2)/225, 30, lower = FALSE)

############################ R Code  6.16  #########################
 
m <- 20000
n <- 15
set.seed(302)
varNC14 <- numeric(m)  # allocate storage space
varEC14 <- numeric(m)  # allocate storage space
for(i in 1:m){varNC14[i] <- var(rnorm(n))}
for(i in 1:m){varEC14[i] <- var(rexp(n))}
NC14 <- (n - 1)*varNC14 / 1^2
EC14 <- (n - 1)*varEC14 / 1^2
n <- 100
varNC99 <- numeric(m)  # allocate storage space
varEC99 <- numeric(m)  # allocate storage space
for(i in 1:m){varNC99[i] <- var(rnorm(n))}
for(i in 1:m){varEC99[i] <- var(rexp(n))}
NC99 <- (n - 1)*varNC99 / 1^2
EC99 <- (n - 1)*varEC99 / 1^2

############################ R Code  6.17  #########################

############################ Figure  6.7   #########################


BDF <- data.frame(RVs = c(NC14, EC14, NC99, EC99),
                 Type = rep(rep(c("Normal", "Exponential"), each=m), 2),
                 Size = c(rep("n = 15", 2*m), rep("n = 100", 2*m)) )
BDF$Size <- factor(BDF$Size, levels = c("n = 15", "n = 100"))
BDF$Type <- factor(BDF$Type, levels = c("Normal", "Exponential"))
p <- ggplot(data = subset(BDF, Size =="n = 15"), aes(x = RVs)) +
 geom_density(fill = "skyblue") +
 facet_grid(Type ~ Size) +
 coord_cartesian(xlim = c(0, 60)) +
 labs(x = expression((n - 1)*s^2/sigma^2), y = "") +
 theme_bw()
p + stat_function(fun = dchisq, args = list(14), col ="blue", n = 500,
                 size = 1, fill = "blue", alpha = 0.2, geom = "area")
p <- ggplot(data = subset(BDF, Size =="n = 100"), aes(x = RVs)) +
 geom_density(fill = "skyblue") +
 facet_grid(Type ~ Size) +
 coord_cartesian(xlim = c(0, 200)) +
 labs(x = expression((n - 1)*s^2/sigma^2), y = "") +
 theme_bw()
p + stat_function(fun = dchisq, args = list(99), col ="blue", n = 500,
                 size = 1, fill = "blue", alpha = 0.2, geom = "area")

############################ R Code 6.18  #########################

nc14 <- c(mean(varNC14), var(varNC14), mean(NC14), var(NC14))
ec14 <- c(mean(varEC14), var(varEC14), mean(EC14), var(EC14))
nc99 <- c(mean(varNC99), var(varNC99), mean(NC99), var(NC99))
ec99 <- c(mean(varEC99), var(varEC99), mean(EC99), var(EC99))
MAT <- rbind(nc14, ec14, nc99, ec99)
colName <- c("E(S^2)", "Var(S^2)", "E(X^2)", "Var(X^2)")
rowName <- c("NC14", "EC14", "NC99", "EC99")
dimnames(MAT) <- list(rowName, colName)
print(MAT)


############################ Figure 6.8  #########################

curve(dt(x, Inf), -6, 6, axes = FALSE, ann = FALSE, n = 500, lwd = 2, 
      lty = "solid", ylim = c(0, 0.45))
curve(dt(x, 3), -6, 6, add = TRUE, n = 500, lty = "dotted", lwd = 2)
curve(dt(x, 1), -6, 6, add = TRUE, n = 500, lty = "dashed", lwd = 2)
axis(side = 1, at = c(-6, -4, -2, 0, qt(0.80, 1), 4, 6), 
     labels=c(-6, -4, -2, 0, "$t_{0.80, 1}$", 4, 6))
# axis(side = 2)
segments(-6, 0, 6, 0, lwd = 2)
arrows(4, .4, 0.8, .3, length = .05)
arrows(4, .35, .85, .25, length = .05)
arrows(4, .3, .82, .2,length = .05)
text(4.4, .41, "$t_{\\infty}")
text(4.3, .36, "$t_{3}$")
text(4.3, .31, "$t_{1}$")
segments(qt(0.80, 1), 0, qt(0.80, 1), dt(qt(0.80,1), 1))


############################ Example 6.23  #########################

pt(2, 4) - pt(-2, 4)
 
pnorm(2) - pnorm(-2)


############################ Figure 6.9  #########################


curve(df(x, 2, 4), 0, 6, axes = FALSE, ann = FALSE, n = 500, lwd = 2, 
      lty = "solid", ylim = c(0, 1))
curve(df(x, 4, 9), 0, 6, add = TRUE, n = 500, lty = "dotted", lwd = 2)
curve(df(x, 19, 19), 0, 6, add = TRUE, n = 500, lty = "dashed", lwd = 2)
# axis(side = 1)
axis(side = 1, at = c( qf(.025, 19, 19), qf(.975, 19, 19), 6), 
     labels=c( "$f_{0.025;19,19}$", "$f_{0.975;19,19}$", 6))
# axis(side = 2)
segments(0, 0, 6, 0, lwd = 2)
arrows(3, .8, 1.3, .6, length = .05)
arrows(3, .6, 1.15, .4, length = .05)
arrows(3, .4, 1.35, .22,length = .05)
text(3.4, .81, "$F_{19, 19}$")
text(3.3, .61, "$F_{4, 9}$")
text(3.3, .41, "$F_{2,4}$")
segments(qf(0.025, 19, 19), 0, qf(0.025, 19, 19), df(qf(0.025,19, 19), 19, 19))
segments(qf(0.975, 19, 19), 0, qf(0.975, 19, 19), df(qf(0.975,19, 19), 19, 19))


############################ Example 6.24  #########################

qf(0.95, 5, 10)  # f_{0.95; 5, 10}   
qf(0.05, 5, 10)  # f_{0.05; 5, 10}

############################ Example 6.25  #########################

pf(2.53, 19, 19)   # P(X <= 2.53)
pf(0.40, 19, 19)   # P(X <= 0.40)

############################ R Code 6.19  #########################

qt(0.975, 5)      # t_{0.975; 5}
qt(0.975, 5)^2    # t_{0.975; 5}^2
qf(0.95, 1, 5)    # f_{0.95; 1, 5}

