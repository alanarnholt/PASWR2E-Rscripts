################### Chapter 10 Script
################### 7/24/15 V.1
library(PASWR2)
library(MASS)
library(coin)
library(nortest)
library(boot)
library(gridExtra)
##################### Chapter 10 ##################################

##################### Figure 10.1 #################################

n <- 20
PI <- 0.5
muS <- n*PI
sigmaS <- sqrt(n*PI*(1 - PI))
s <- 0:20
ps <- dbinom(s, n, PI)
plot(s, ps, type = "h", ylim = c(0, 0.20), xlab = "$s$", ylab = "$\\gil{P}(S = s)$")
points(s, ps, cex = 0.5)
lines(x = c(0, 20), y = c(0, 0), lwd = 3)
curve(dnorm(x, muS, sigmaS), from = 0, to = 20, add = TRUE, n = 500)


##################### Figure 10.2 #################################

with(data = PHONE, eda(call.time))

##################### R Code 10.1 #################################

pvalue <- sum(dbinom(0:21, 21, 0.5)[dbinom(0:21, 21, 0.5) <= dbinom(11, 21, 0.5)] )
pvalue
 
 

##################### Example 10.1 #################################

1 - 2*pbinom(5:8, 23, 0.5)

with(data = PHONE, SIGN.test(call.time, md = 2.1))
 

##################### Example 10.3 #################################

PH <- c(7.2, 7.3, 7.3, 7.4)        # Enter data
DIFF <- PH - 7.25                  # Create differences (DIFF)
absD <- abs(DIFF)                  # Absolute value of DIFF (absD)
rankabsD <- rank(absD)             # Rank the absD values
signD <- sign(DIFF)                # Store the signs of DIFF
signrank <- rankabsD*signD         # Create a vector of signed ranks
tp <- sum(signrank[signrank>0])    # Calculate t+
tp


n <- length(DIFF)
signs <- as.matrix(expand.grid(d1 = 0:1, d2 = 0:1, d3 = 0:1, d4 = 0:1))
signs                             # 1s represent positive ranks
mat <- matrix(rankabsD)           # Put rankabsD in matrix form
mat
 
Tp <- signs%*%mat              # (16X4)*(4X1) = 16X1 vector of T+
Tp <- sort(Tp)                 # Sort the distribution of T+
SampDist <- table(Tp)/2^n
library(MASS)                  # used for fractions()
fractions(SampDist)            # Sampling distribution of T+
 
pvalue <- sum(Tp>=tp)/2^n     # Calculate p-value
fractions(pvalue)
 
wilcoxe.test(PH, mu = 7.25, alternative = "greater")

##################### Figure 10.3 #################################

ggplot(data = WAIT, aes(x = minutes)) + 
  geom_density(fill = "lightblue") + theme_bw() + labs(x = "Waiting Time (minutes)", y = "")

##################### R Code 10.2 #################################

with(data = WAIT, wilcox.test(minutes, mu=6, alternative="less"))
 
n2means <- apply(combn(WAIT$minutes, 2), 2, mean)  # n choose 2 means
WalshAverages <- c(WAIT$minutes, n2means)
 
qsignrank(0.05, 15)
 
psi <- psignrank(28:33, 15)
names(psi) <- 28:33
psi
 
SWA <- sort(WalshAverages)  # sort values
SWA[90]
 
with(data = WAIT, 
     wilcox.test(minutes, mu = 6, alternative = "less", conf.int = TRUE)
    )
 
with(data = WAIT, 
     wilcoxe.test(minutes, mu = 6, alternative = "less")
     )


##################### Figure 10.4 #################################

tp <- 0:120
ps <- dsignrank(tp, 15)
plot(tp, ps, type = "h", ylim = c(0, 0.025), xlab = "$t^+$", ylab = "$\\gil{P}(T^+ = t^+)$")
points(tp, ps, cex = 0.25)
lines(x = c(0, 120), y = c(0, 0), lwd = 3)
mut <- 15*(15+1)/4
st <- sqrt(15*(15+1)*(2*15+1)/24)
curve(dnorm(x, mut,  st), from = 0, to = 120, add = TRUE, n = 500)


##################### Figure 10.5 #################################

d <- AGGRESSION$violence - AGGRESSION$noviolence
DF <- data.frame(x = d)
ggplot(data = DF, aes(x = x)) + 
  geom_density(fill = "lightblue") + theme_bw() + labs(x = "Differences (violence - noviolence)", y = "")


##################### R Code 10.3 #################################

with(data = AGGRESSION,  wilcox.test(violence, noviolence, 
    paired = TRUE, alternative = "greater")
)


##################### R Code 10.4 #################################

with(data = AGGRESSION, wilcoxe.test(violence, noviolence, 
     paired = TRUE, alternative = "greater")
)
  

PD <- AGGRESSION$violence - AGGRESSION$noviolence
n2means <- apply(combn(PD, 2), 2, mean)  # n choose 2 means
SWA <- sort(c(PD, n2means))              # Sorted Walsh averages
n <- length(PD)
k <- 0.5 + n*(n+1)/4 + qnorm(.05)*sqrt(n*(n+1)*(2*n+1)/24)
k <- floor(k)
k
SWA[k]                                   # kth Walsh average
 
ADD <- outer(PD, PD, "+")
SWA2 <- sort(ADD[!lower.tri(ADD)])/2
SWA2[k]                            # kth Walsh average 

##################### Example 10.6 #################################

x <- c(2, 5)
y <- c(9, 12, 14)
n <- length(x)
m <- length(y)
N <- n + m
r <- rank(c(x, y))
w <- sum(r[seq(along = x)])                     # observed w value
u <- sum(r[seq(along = x)]) - n*(n + 1)/2       # observed u value
val <- combn(r, n)                    # possible rankings for X
W <- apply(val, 2, sum)               # W values
U <- W - n*(n + 1)/2                  # U values
display <- rbind(val, W, U)           # X rankings with W and U
display
 
xtabs(~W)/choose(5, 2)      # Sampling distribution of W
 
xtabs(~U)/choose(5, 2)              # Sampling distribution of U
dwilcox(0:6, 2, 3)                  # Produces distribution of U in R

##################### Example 10.7 #################################

x <- c(7.2, 7.2, 7.3, 7.3)
y <- c(7.3, 7.3, 7.4, 7.4)
n <- length(x)
m <- length(y)
N <- n + m
r <- rank(c(x, y))
w <- sum(r[seq(along = x)])               # observed w value
w
val <- combn(r, n)                        # possible rankings
W <- apply(val, 2, sum)                   # W values
xtabs(~W) / choose(8, 4)


pvalue <- 2*(sum(W <= w)/choose(N, n))
pvalue
# OR
pvalue1 <- 2 * mean(W <= w)
pvalue1
 
wilcoxe.test(x, y)

##################### R Code 10.5 #################################


##################### Figure 10.6 #################################

A <- c(1.2, 1.5, 2.3, 4.3)
B <- c(4.5, 5.7, 6.1, 8.6)
DF <- stack(data.frame(A, B))
ggplot(data = DF, aes(x = values)) +
 geom_density(aes(fill = ind), alpha = 0.5) +
 theme_bw() +
 labs(y = "", x = "weight gain (pounds)") +
 guides(fill = guide_legend("Diet \nType")) +
 scale_fill_grey()

ggplot(data = DF, aes(x = ind, y = values, fill = ind)) +
 geom_boxplot() +
 coord_flip() +
 theme_bw() +
 labs(x = "", y = "weight gain (pounds)") +
 guides(fill = guide_legend("Diet \nType")) +
 scale_fill_grey(start = 0.3, end = 0.7)


##################### R Code 10.6 #################################
  
n <- length(A)
m <- length(B)
pwilcox(1:(n*m), n, m)


##################### R Code 10.7 #################################
 
pwil <- pwilcox(1:(n*m), n, m)  
k <- which(pwil >= 0.05)[1]
k

##################### R Code 10.8 #################################
 
diffs <- matrix(sort(outer(A, B,"-")), byrow = FALSE, nrow = 4)
diffs
CL <- 1 - 2*pwilcox((k - 1), n, m)
CL
CI <- c(diffs[k], diffs[n*m-k+1])
CI
 
wilcox.test(A, B, conf.int = TRUE, conf.level = 0.90)  

wilcoxe.test(A, B)


##################### Figure 10.7 #################################

n <- 10
m <- 10
N <- n + m
muW <- n*(N + 1)/2
sigW <- sqrt(n*m*(N + 1)/12)
c(muW, sigW)
c(n*(n+1)/2, n*(2*N-n+1)/2)
### so U
c(n*(n+1)/2 - n*(n+1)/2, n*(2*N-n+1)/2 - n*(n+1)/2)
### can only get dist of U with dwilcox()...must shift xs for dist of W
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 1), mar = c(4.1, 4.1, 1.1, 1.1))
plot(55:155, dwilcox(0:100, 10, 10), type = "h", xlab = "$w$", ylab = "$\\gil{P}(W = w)$") # W
points(55:155, dwilcox(0:100, 10, 10), cex = 0.25)
curve(dnorm(x, muW, sigW), from = 55, to = 155, n = 500, add = TRUE)
plot(0:100, dwilcox(0:100, 10, 10), type = "h", xlab = "$u$", ylab = "$\\gil{P}(U = u)$")  # U
points(0:100, dwilcox(0:100, 10, 10), cex = 0.25)
curve(dnorm(x, n*m/2, sigW), from = 0, to = 100, n = 500, add = TRUE)
par(opar)


##################### Figure 10.8 #################################

ggplot(data = SWIMTIMES, aes(x = diet, y = seconds, fill = diet)) + 
  geom_boxplot() + theme_bw() + guides(fill = FALSE) + 
  scale_fill_grey(start = 0.3, end = 0.7) + labs(y = "Time Improvement (seconds)")
 

##################### Example 10.9 #################################

wilcox.test(seconds~diet, data = SWIMTIMES)
 

library(coin)
wilcox_test(seconds ~ diet, data = SWIMTIMES, distribution = "exact", conf.int = TRUE, conf.level = 0.90)
 
xtabs(~SWIMTIMES$diet)
n <- xtabs(~SWIMTIMES$diet)[1]
m <- xtabs(~SWIMTIMES$diet)[2]
N <- n + m
highfat <- SWIMTIMES$seconds[SWIMTIMES$diet == "highfat"]
lowfat <- SWIMTIMES$seconds[SWIMTIMES$diet == "lowfat"]
diffs <-sort(outer(highfat, lowfat, "-"))
k <- 0.5 + n * m/2 + qnorm(0.05)*sqrt(n * m * (N + 1)/12) 
k <- floor(k)
names(k) <- NULL
k
CI <- c(diffs[k], diffs[n*m-k+1])  # 90% CI
CI


##################### R Code 10.9 #################################

Method1 <- c(6, 1, 2, 0, 0, 1, 1, 3, 1, 2, 1, 2, 4, 2, 1, 1, 1, 3, 7, 1)
Method2 <- c(3, 2, 1, 2, 1, 6, 2, 1, 1, 2, 1, 1, 2, 3, 2, 2, 3, 2, 5, 2)
Method3 <- c(2, 1, 2, 3, 2, 2, 4, 3, 2, 3, 2, 5, 1, 1, 3, 7, 6, 2, 2, 2)
Method4 <- c(2, 1, 1, 3, 1, 2, 1, 6, 1, 1, 0, 1, 1, 1, 1, 2, 2, 1, 5, 4)
DF <- stack(data.frame(Method1, Method2, Method3, Method4))
ggplot(data = DF, aes(x = ind, y = values, fill = ind)) + 
  geom_boxplot() + 
  guides(fill = FALSE) + 
  labs(x = "", y = "Number of successful free-throws") + 
  coord_flip() +
  scale_x_discrete(limits = rev(levels(DF$ind))) +
  scale_fill_grey(start = 0.3, end = 0.7) +
  theme_bw() 
ggplot(data = DF, aes(x = values)) + 
  geom_density(aes(fill = ind)) + 
  facet_grid(ind ~ .) + 
  labs(y = "", x = "Number of successful free-throws") + 
  guides(fill = FALSE) +
  scale_fill_grey(start = 0.3, end = 0.7) + 
  theme_bw() 
 

##################### R Code 10.10 #################################

RR <- qchisq(0.95,3)                      # Rejection region
RR
DF$RKs <- rank(DF$values)                 # Ranks for all
MRKs <- unstack(DF, RKs ~ ind)
MRKs[1:5,]                                # Show first five rows
RK <- apply(MRKs, 2, mean)                # Treatment ranks
names(RK) <- c("MRKT1", "MRKT2", "MRKT3", "MRKT4")
RK
MRK <- mean(RK)                           # Overall mean rank
MRK
N <- length(DF$RKs)
n1 <- length(Method1)
n2 <- length(Method2)
n3 <- length(Method3)
n4 <- length(Method4)
Hobs <- 12*(n1*(RK[1] - MRK)^2 + n2*(RK[2] - MRK)^2 + 
            n3*(RK[3] - MRK)^2 + n4*(RK[4] - MRK)^2)/(N * (N + 1))
names(Hobs) <- "statistic"
Hobs
tj <- xtabs(~RKs, data = DF)
tj
CF <- 1-(sum(tj^3 - tj)/(N^3 - N))       # correction factor
Hc <- Hobs/CF                            # corrected statistic
hs <- c(Hobs, Hc)
hs
pval <- 1-pchisq(hs, 3)
names(pval) <- c("pvalueHobs","pvalueHc")
pval
 
kruskal.test(values ~ ind, data = DF)   
 

##################### R Code 10.11 #################################

a <- 4                                   # Four methods
alpha <- 0.20
Z12 <- abs(RK[1] - RK[2])/sqrt((N*(N + 1)/12)*(1/n1 + 1/n2))
Z13 <- abs(RK[1] - RK[3])/sqrt((N*(N + 1)/12)*(1/n1 + 1/n3))
Z14 <- abs(RK[1] - RK[4])/sqrt((N*(N + 1)/12)*(1/n1 + 1/n4))
Z23 <- abs(RK[2] - RK[3])/sqrt((N*(N + 1)/12)*(1/n2 + 1/n3))
Z24 <- abs(RK[2] - RK[4])/sqrt((N*(N + 1)/12)*(1/n2 + 1/n4))
Z34 <- abs(RK[3] - RK[4])/sqrt((N*(N + 1)/12)*(1/n3 + 1/n4))
Zij <- c(Z12, Z13, Z14, Z23, Z24, Z34)
names(Zij) <- c("Z12","Z13","Z14","Z23","Z24","Z34")
CV <- qnorm(1- alpha/(a*(a - 1)))
Zij
CV
which(Zij > CV)


##################### R Code 10.12 #################################

##################### Figure 10.10 #################################

DF <- stack(HSWRESTLER[,c('hwfat', 'tanfat', 'skfat')])
head(DF)
ggplot(data = DF, aes(x = ind, y = values, fill = ind)) + 
  geom_boxplot() + 
  guides(fill = FALSE) + 
  labs(x = "", y = "Percent body fat") + 
  coord_flip() +
  scale_x_discrete(limits = rev(levels(DF$ind))) +
  scale_fill_grey(start = 0.3, end = 0.7) +
  theme_bw() 

ggplot(data = DF, aes(x = values)) + 
  geom_density(aes(fill = ind)) + 
  facet_grid(ind ~ .) + 
  labs(y = "", x = "Number of successful free-throws") + 
  guides(fill = FALSE) +
  scale_fill_grey(start = 0.3, end = 0.7) + 
  theme_bw()
 

##################### R Code 10.13 #################################

cfat <- HSWRESTLER[, c('hwfat', 'tanfat', 'skfat')]
RK <- t(apply(cfat, 1, rank))  # rankings
OBSandRK <- cbind(cfat, RK)
OBSandRK[1:5, ]
Rj <- apply(RK, 2, sum)
b <- dim(HSWRESTLER)[1]
k <- dim(cfat)[2]
S <- (12/(b*k*(k + 1)))*sum(Rj^2) - 3*b*(k + 1)
S
pval <- pchisq(S, k-1, lower = FALSE)
pval

##################### R Code 10.14 #################################

DF$Block <- factor(rep(1:78, 3))  # create 78 blocks
head(DF)  # show first 6 rows of DF
friedman.test(values ~ ind | Block, data = DF)
 

##################### R Code 10.15 #################################

alpha <- 0.20
ZR12 <- abs(Rj[1] - Rj[2])/sqrt(b*k*(k + 1)/6)
ZR13 <- abs(Rj[1] - Rj[3])/sqrt(b*k*(k + 1)/6)
ZR23 <- abs(Rj[2] - Rj[3])/sqrt(b*k*(k + 1)/6)
CV <- qnorm(1 - alpha/(k*(k - 1)))
ZRij <- c(ZR12, ZR13, ZR23)
names(ZRij) <- c("ZR12","ZR13","ZR23")
ZRij
CV
which(ZRij > CV)
 

##################### Example 10.12 #################################

xtabs(~goals, data = SOCCER)
 
PX <- c(dpois(0:5, 2.5), ppois(5, 2.5, lower = FALSE))
PX
 
EX <- 232*PX
OB <- c(as.vector(xtabs(~goals, data = SOCCER)[1:6]), 
        sum(xtabs(~goals, data = SOCCER)[7:9]))
OB
ans <- cbind(PX, EX, OB)
row.names(ans) <- c(" X=0"," X=1"," X=2"," X=3"," X=4"," X=5","X>=6")
ans
 

##################### R Code 10.16 #################################

chi.obs <- sum((OB-EX)^2/EX)
chi.obs
 
p.val <- pchisq(chi.obs, 7-1, lower = FALSE)
p.val
 
chisq.test(x = OB, p = PX)

##################### R Code 10.17 #################################

##################### Figure 10.11 #################################

hist(SOCCER$goals, breaks = c((-0.5 + 0):(8 + 0.5)), col = "lightblue", 
     xlab = "Goals scored", ylab = "", freq = TRUE, main = "")
x <- 0:8
fx <- (dpois(0:8, lambda = 2.5))*232
lines(x, fx, type = "h")
lines(x, fx, type = "p", pch = 16)
 

##################### Example 10.13 #################################

mu <- mean(GRADES$sat)
sig <- sd(GRADES$sat) 
c(mu, sig)
 
bin <- seq(from = mu - 3*sig, to = mu + 3*sig, by = sig)
round(bin, 0)                     # vector of bin cut points
T1 <- table(cut(GRADES$sat, breaks = bin))
T1                                # count of observations in bins
OB <- as.vector(T1)
OB                                # vector of observations
PR <- c(pnorm(-2), pnorm(-1:2) - pnorm(-2:1), 
        pnorm(2, lower = FALSE))  # area under curve
EX <- 200*PR                      # Expected count in bins
ans <- cbind(PR, EX, OB)          # column bind values in ans
ans
 

chi.obs <- sum((OB-EX)^2/EX)
chi.obs
 
p.val <- pchisq(chi.obs, 6 - 2 - 1, lower = FALSE)
p.val

##################### R Code 10.18 #################################

chisq.test(x=OB, p=PR)  # returns incorrect dof and p-value

##################### R Code 10.19 #################################

##################### Figure 10.12 #################################

hist(GRADES$sat, breaks = bin, col = "lightblue", 
     xlab = "SAT scores", ylab="", freq = TRUE, main = "")
x <- bin[2:7] - sig/2
fx <- PR*200
lines(x, fx, type = "h")
lines(x, fx, type = "p", pch = 16)
 
##################### Example 10.14 #################################

x <- 5:9
mu <- 6.5
sig <- sqrt(2)
x <- sort(x)
n <- length(x)
FoX <- pnorm(x, mean = mu, sd = sig)
FoX
 
FnX <- seq(1:n)/n
Fn1X <- (seq(1:n) - 1)/n
DP <- (FnX - FoX)
DM <- FoX - Fn1X
Dp <- abs(DP)
Dm <- abs(DM)
EXP <- cbind(x, FnX, Fn1X, FoX, Dp, Dm)
Mi <-apply(EXP[, c(5,6)],1, max)
TOT <- cbind(EXP, Mi)
TOT
Dn <- max(Mi)
Dn
 
##################### R Code 10.20 #################################

ks.test(x, y = "pnorm", mean = mu, sd = sig)

##################### R Code 10.21 #################################

ksdist <- function (n = 10, sims = 10000, alpha = 0.05){
    Dn <- replicate(sims, ks.test(rnorm(n), pnorm)$statistic)
    cv <- quantile(Dn, 1 - alpha)
    plot(density(Dn), col = "blue", lwd = 2, main = "",
    xlab = paste("Simulated critical value =", round(cv, 3),
    "for n =", n, "when the alpha value =", alpha))
    title(
    main = list(expression(paste("Simulated Sampling Distribution of ",
    D[n]))))
}

##################### Figure 10.14 #################################

set.seed(13)
KSDIST <- function (n = 10, sims = 10000, alpha = 0.05){
    Dn <- replicate(sims, ks.test(rnorm(n), pnorm)$statistic)
    cv <- quantile(Dn, 1 - alpha)
    plot(density(Dn), col = "blue", lwd = 2, main = "",
    xlab = paste("Simulated critical value =", round(cv, 3),
    "for n =", n, "when the alpha value =", alpha))
    title(main = "Simulated Sampling Distribution of $D_n$")
    pvalue <- mean(Dn >= .25558)
    pvalue
}
KSDIST(n = 5, sims = 10000, alpha = 0.05)
 
##################### R Code 10.22 #################################

ksLdist <- function (n = 10, sims = 1000, alpha = 0.05){
   Dn <- c()
   DnL <- c()
   for (i in 1:sims) {
       x <- rnorm(n)
       mu <- mean(x)
       sig <- sd(x)
       Dn[i] <- ks.test(x, pnorm)$statistic
       DnL[i] <- ks.test(x, pnorm, mean = mu, sd = sig)$statistic
   }
   ys <- range(density(DnL)$y)
   xs <- range(density(Dn)$x)
   cv <- quantile(Dn, 1 - alpha)
   cvp <- quantile(DnL, 1 - alpha)
   plot(density(Dn, bw = 0.02), col="blue", lwd=2, ylim=ys, xlim=xs,
       main = "", , xlab="", sub = paste("Simulated critical value =",
       round(cv, 3), "(simple hypothesis) and ", round(cvp, 3),
         "(composite hypothesis)\n for n =", n,"when the alpha value =",
         alpha))
   title(
   main = list(expression(paste("Simulated Sampling Distribution of ",
   D[n]))))
   lines(density(DnL, bw = 0.02), col = "red", lwd = 2, lty = 2)
   legend(mean(xs), max(ys), legend = c("Simple Hypothesis",
       "Composite Hypothesis"), col = c("blue", "red"), xjust = 0,
       text.col = c("black", "black"), lty = c(1, 2), bg = "gray95",
       cex = 1, lwd = 2)
   box()
   abline(h = 0)
}

##################### Figure 10.15 #################################

 set.seed(13)
KSLDIST <- function (n = 10, sims = 1000, alpha = 0.05){
    Dn <- c()
    DnL <- c()
    for (i in 1:sims) {
        x <- rnorm(n)
        mu <- mean(x)
        sig <- sd(x)
        Dn[i] <- ks.test(x, pnorm)$statistic
        DnL[i] <- ks.test(x, pnorm, mean = mu, sd = sig)$statistic
    }
    ys <- range(density(DnL)$y)
    xs <- range(density(Dn)$x)
    cv <- quantile(Dn, 1 - alpha)
    cvp <- quantile(DnL, 1 - alpha)
    plot(density(Dn, bw = 0.02), col="blue", lwd=2, ylim=ys, xlim=xs,
        main = "", , xlab="", sub = paste("Simulated critical value =",
        round(cv, 3), "(simple hypothesis) and ", round(cvp, 3),
          "(composite hypothesis)\n for n =", n,"when the alpha value =",
          alpha))
    title(main = "Simulated Sampling Distribution of $D_n$")
    lines(density(DnL, bw = 0.02), col = "red", lwd = 2, lty = 2)
    legend(mean(xs), max(ys), legend = c("Simple Hypothesis",
        "Composite Hypothesis"), col = c("blue", "red"), xjust = 0,
        text.col = c("black", "black"), lty = c(1, 2), bg = "gray95",
        cex = 1, lwd = 2)
    box()
    abline(h = 0)
}
KSLDIST(sims = 10000, n = 10)

##################### R Code 10.23 #################################
 
library(nortest) 
lillie.test(PHONE$call.time)

##################### R Code 10.24 #################################

DWA <- function(Dn = 0.3, n = 10){
 p.value <- exp(-7.01256*Dn^2*(n + 2.78019)
 + 2.99587*Dn*(n + 2.78019)^0.5 - 0.122119
 + 0.974598/n^.5 + 1.67997/n)
 names(p.value) <- NULL
 round(p.value, 4) 
}
DWA(Dn = 0.191, n = 23)

##################### Example 10.16 #################################

x <- c(47, 50, 57, 54, 52, 54, 53, 65, 62, 67, 69, 74, 51, 57, 57, 59)
shapiro.test(x)
 
##################### 10.8.1 Test of Independence #####################

HA <- c(110, 277, 50, 163, 302, 63)
HAT <- matrix(data = HA, nrow = 2, byrow = TRUE)
dimnames(HAT) <- list(SEX = c("Male", "Female"),
    Category = c("Very Happy", "Pretty Happy", "Not To Happy"))
HAT
E <- outer(rowSums(HAT), colSums(HAT), "*")/sum(HAT)
E
# OR
chisq.test(HAT)$expected
 
chi.obs <- sum((HAT - E)^2/E )
chi.obs
 
p.val <- pchisq(chi.obs, 2, lower = FALSE)
p.val
 
chisq.test(HAT)

##################### 10.8.2 Test of Homogeneity #################################

DT <- c(67, 76, 57, 48, 73, 79)
DTT <- matrix(data = DT, nrow = 2, byrow = TRUE)
dimnames(DTT) <- list(Treatment = c("Drug", "Placebo"),
   Category = c("Improve", "No Change", "Worse"))
DTT
E <- chisq.test(DTT)$expected
E
 
chi.obs <- sum((DTT - E)^2/E )
chi.obs
 
p.val <- pchisq(chi.obs, 2, lower = FALSE)
p.val

chisq.test(DTT)

##################### R Code 10.25 #################################

set.seed(14)
alpha <- 1
lambda = 1/3
MX <- alpha/lambda
VX <- alpha/lambda^2
n <- 100
rsg1 <- rgamma(n, alpha, lambda)
rsg2 <- rgamma(n, alpha, lambda)
rsg3 <- rgamma(n, alpha, lambda)
B <- 10^4 - 1
ThetaHatStar1 <- numeric(B)
ThetaHatStar2 <- numeric(B)
ThetaHatStar3 <- numeric(B)
for (i in 1:B) {
  bootsample1 <- sample(rsg1, size = n, replace = TRUE)
  bootsample2 <- sample(rsg2, size = n, replace = TRUE)
  bootsample3 <- sample(rsg3, size = n, replace = TRUE)
  ThetaHatStar1[i] <- mean(bootsample1)
  ThetaHatStar2[i] <- mean(bootsample2)
  ThetaHatStar3[i] <- mean(bootsample3)
}

##################### R Code 10.26 #################################

library(boot)
GammaMean <- function(data, i){
  d <- data[i]
  M <- mean(d)
}
set.seed(14)
B <- 10^4 - 1
b.obj1 <- boot(data = rsg1, statistic = GammaMean, R = B)
b.obj2 <- boot(data = rsg2, statistic = GammaMean, R = B)
b.obj3 <- boot(data = rsg3, statistic = GammaMean, R = B)

##################### R Code 10.27 #################################

BootBias1 <- mean(ThetaHatStar1) - mean(rsg1)
BootBias2 <- mean(ThetaHatStar2) - mean(rsg2)
BootBias3 <- mean(ThetaHatStar3) - mean(rsg3)
c(BootBias1, BootBias2, BootBias3)

##################### R Code 10.28 #################################

sigxbar <- sqrt(alpha/(n*lambda^2))
sigxbar
c(sd(ThetaHatStar1), sd(ThetaHatStar2), sd(ThetaHatStar3))

##################### R Code 10.29 #################################

BootBias1b <- mean(b.obj1$t) - b.obj1$t0
BootBias2b <- mean(b.obj2$t) - b.obj2$t0
BootBias3b <- mean(b.obj3$t) - b.obj3$t0
c(BootBias1b, BootBias2b, BootBias3b)
c(sd(b.obj1$t), sd(b.obj2$t), sd(b.obj3$t))

##################### R Code 10.30 #################################

b.obj1
b.obj2
b.obj3

##################### R Code 10.31 #################################

##################### Figure 10.18 #################################


library(ggplot2)
p1 <- ggplot(data = data.frame(x = c(0, MX + 4*sqrt(VX))), aes(x = x)) + 
  stat_function(fun = dgamma, args = list(alpha, lambda)) + 
  labs(x = "$X \\sim \\Gamma(1, 1/3)$", y = "") +
  geom_vline(xintercept = MX, lty = "solid", col = "blue") +
  ylim(c(0, 0.35)) +
  theme_bw()
# p1
p2 <- ggplot(data = data.frame(x = c(MX - 5*sqrt(VX/n), MX + 5*sqrt(VX/n))), aes(x = x)) + 
  stat_function(fun = dgamma, args = list(n*alpha, n*lambda)) + 
  geom_vline(xintercept = MX, lty = "solid", col = "blue") + 
  labs(x = "$\\Mean{X} \\sim \\Gamma(100, 100/3)$", y = "") + 
  ylim(c(0, 1.5)) +
  theme_bw()
# p2
p3 <- ggplot(data = data.frame(x = rsg1), aes(x = x)) +
  geom_density(fill = "lightblue") + 
  geom_vline(xintercept = c(MX, mean(rsg1)), lty = c("solid","dashed"), col = c("blue", "red")) +
  xlim(c(0, MX + 4*sqrt(VX))) + 
  labs(x = "$x$", y = "") + 
  ylim(c(0, 0.35)) +
  # stat_function(fun = dgamma, args = list(alpha, lambda)) +
  theme_bw()
# p3
p4 <- ggplot(data = data.frame(x = ThetaHatStar1), aes(x = x)) +
  geom_density() + 
  xlim(c(MX - 5*sqrt(VX/n), MX + 5*sqrt(VX/n))) +
  geom_vline(xintercept = c(MX, mean(ThetaHatStar1)), lty = c("solid", "dashed"), col = c("blue", "red")) + 
  labs(x = "$\\bar{x}^{*}$", y = "") + 
  ylim(c(0, 1.5)) +
  theme_bw()
# p4
p5 <- ggplot(data = data.frame(x = rsg2), aes(x = x)) +
  geom_density(fill = "lightblue") + 
  geom_vline(xintercept = c(MX, mean(rsg2)), lty = c("solid","dashed"), col = c("blue", "red")) +
  xlim(c(0, MX + 4*sqrt(VX))) + 
  labs(x = "$x$", y = "") + 
  ylim(c(0, 0.35)) +
  # stat_function(fun = dgamma, args = list(alpha, lambda)) +
  theme_bw()
# p5
p6 <- ggplot(data = data.frame(x = ThetaHatStar2), aes(x = x)) +
  geom_density() + 
  xlim(c(MX - 5*sqrt(VX/n), MX + 5*sqrt(VX/n))) +
  geom_vline(xintercept = c(MX, mean(ThetaHatStar2)), lty = c("solid", "dashed"), col = c("blue", "red")) + 
  labs(x = "$\\bar{x}^{*}$", y = "") + 
  ylim(c(0, 1.5)) +
  theme_bw()
# p6
p7 <- ggplot(data = data.frame(x = rsg3), aes(x = x)) +
  geom_density(fill = "lightblue") + 
  geom_vline(xintercept = c(MX, mean(rsg3)), lty = c("solid","dashed"), col = c("blue", "red")) +
  xlim(c(0, MX + 4*sqrt(VX))) + 
  labs(x = "$x$", y = "") + 
  ylim(c(0, 0.35)) +
  # stat_function(fun = dgamma, args = list(alpha, lambda)) +
  theme_bw()
# p7
p8 <- ggplot(data = data.frame(x = ThetaHatStar3), aes(x = x)) +
  geom_density() + 
  xlim(c(MX - 5*sqrt(VX/n), MX + 5*sqrt(VX/n))) +
  geom_vline(xintercept = c(MX, mean(ThetaHatStar3)), lty = c("solid", "dashed"), col = c("blue", "red")) + 
  labs(x = "$\\bar{x}^{*}$", y = "") + 
  ylim(c(0, 1.5)) +
  theme_bw()
# p8
#### put it all together now
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2)


##################### R Code 10.32 #################################

##################### Figure 10.19 #################################


TotalTime <- max(cumsum(SDS4$times))  # Total time in seconds
n <- 40                               # Number of interarrival times
est.lambda <- n/TotalTime             # Estimated lambda for Poisson
est.mean <- 1/est.lambda              # Est. waiting time for next car
ans <- c(TotalTime, n, est.lambda, est.mean)
names(ans) <- c("Total Time", "Number of Cars", "Est Lambda", "Est Mean")
ans
c(mean(SDS4$times), sd(SDS4$times))   # Exponential Check

##################### R Code 10.33 #################################

ggplot(data = SDS4, aes(x = times)) +
 geom_density(fill = "pink", alpha = 0.3) +
 stat_function(fun = dexp, arg = list(est.lambda), lty = "dashed") +
 labs(x = "Interarrival Times (seconds)", y = "") +
 theme_bw()
 
##################### R Code 10.34 #################################

library(boot)
times.mean <- function(data, i){
  d <- data[i]
  M <- mean(d)
  V <- var(d)/40  # sigma^2/n
  c(M, V)
}
B <- 10^4 - 1
set.seed(10)
b.obj <- boot(data = SDS4$times, statistic = times.mean, R = B)
b.obj

##################### R Code 10.35 #################################

DF <- data.frame(ths = b.obj$t)
p <- ggplot(data = DF, aes(x = ths.1)) +
 geom_density(fill = "lightblue") +
 labs(x = "$\\hat{\\theta}^{*}$", y = "") +
 theme_bw()
p

p1 <- ggplot(data = DF, aes(sample = ths.1)) +
 stat_qq() +
 theme_bw() +
 coord_fixed()  # 1:1 scaling between axes
p1
 
##################### R Code 10.36 #################################

boot.ci(b.obj, conf = 0.95, type = c("norm", "basic", "perc",
                                     "bca", "stud"))

##################### R Code 10.37 #################################

ThetaHat <- b.obj$t0[1]
ThetaHat
Bias <- mean(b.obj$t[,1]) - b.obj$t0[1]
Bias
alpha <- 0.05
NBCI <- (ThetaHat - Bias) + c(-1, 1)*qnorm(1 - alpha/2)*sd(b.obj$t[,1])
NBCI

##################### R Code 10.38 #################################

BBCI <- c(2*ThetaHat - sort(b.obj$t[,1])[(B + 1)*(1 - alpha/2)], 
          2*ThetaHat - sort(b.obj$t[,1])[(B + 1)*(alpha/2)])
BBCI

##################### R Code 10.39 #################################

PBCI <- c(sort(b.obj$t[,1])[(B + 1)*(alpha/2)], 
          sort(b.obj$t[,1])[(B + 1)*(1 - alpha/2)])
PBCI

##################### R Code 10.40 #################################

z <- qnorm(sum(b.obj$t[,1] < ThetaHat)/B)  # bias factor
z
n <- length(SDS4$times)
u <- numeric(n)
for(i in 1:n){
  u[i] <- mean(SDS4$times[-i])
}
ubar <- mean(u)
numa <- sum((ubar - u)^3)
dena <- 6*sum((ubar - u)^2)^(3/2)
a <- numa/dena  # skewness correction factor
a
a1 <- pnorm(z + (z + qnorm(alpha/2))/(1 - a*(z + qnorm(alpha/2))))
a2 <- pnorm(z + (z + qnorm(1 - alpha/2))/(1 - a*(z + qnorm(1 - alpha/2))))
(B + 1)*a1
(B + 1)*a2
### Interpolation
kLower <- floor((B + 1)*a1)
kUpper <- floor((B + 1)*a2)
c(kLower, kUpper)
ll <- sort(b.obj$t[,1])[kLower] + (qnorm(a1) - qnorm(kLower/(B + 1))) / 
  (qnorm((kLower + 1)/(B + 1)) - qnorm(kLower/(B + 1))) * 
  (sort(b.obj$t[,1])[kLower + 1] - sort(b.obj$t[,1])[kLower])
ul <- sort(b.obj$t[,1])[kUpper] + (qnorm(a2) - qnorm(kUpper/(B + 1))) / 
  (qnorm((kUpper + 1)/(B + 1)) - qnorm(kUpper/(B + 1))) * 
  (sort(b.obj$t[,1])[kUpper + 1] - sort(b.obj$t[,1])[kUpper])
BCaCI <- c(ll, ul)
BCaCI

##################### R Code 10.41 #################################

TS <- (b.obj$t[,1] - b.obj$t0[1])/sqrt(b.obj$t[ ,2])
CT <- sort(TS)[c((B + 1)*0.025, (B + 1)*0.975)]
CT  # critical t values
BTCI <- c(b.obj$t0[1] - CT[2]*sqrt(b.obj$t0[2]), 
        b.obj$t0[1] - CT[1]*sqrt(b.obj$t0[2]))
BTCI
 
##################### R Code 10.42 #################################

library(MASS)
n <- dim(Animals)[1]
B <- 10^4 - 1
R <- numeric(B)
set.seed(2)
for(b in 1:B){
  i <- sample(1:n, size = n, replace = TRUE)
  BRAIN <- Animals$brain[i]
  BODY  <- Animals$body[i]
  R[b] <- cor(log(BRAIN), log(BODY))
}
## Using quantile function
quantile(R, probs = c(0.025, 0.975), type = 2)
## Using formula
RS <- sort(R)
c(RS[(B + 1)*0.025], RS[(B + 1)*0.975])


##################### R Code 10.43 #################################

B <- 10^4 - 1
cor.boot <- function(data, i){
  d <- data[i, ]
  RS <- cor(log(d[ , 1]), log(d[ , 2]))
}
set.seed(1)
b.obj <- boot(data = Animals, statistic = cor.boot, R = B)
b.obj
boot.ci(b.obj, type = "perc")

##################### R Code 10.44 #################################

##################### Figure 10.21 #################################


library(ggplot2)
DF <- data.frame(x = b.obj$t)
p <- ggplot(data = DF, aes(x = x)) +
 geom_density(fill = "pink", alpha = 0.3) +
 labs(x = "$r^*$", y = "") +
 theme_bw()
x.dens <- density(b.obj$t)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
# shade left tail
p + geom_area(data = subset(df.dens, x <= 0.5545 & x >= min(DF$x)),
             aes(x = x, y= y), fill = "blue", alpha = 1) +
# shade right tail
geom_area(data = subset(df.dens, x >= 0.9559 & x <= max(DF$x)),
           aes(x = x, y= y), fill = "blue", alpha = 1)

##################### R Code 10.45 #################################

caseresamp <- function(data, i){
  mod <- lm(hwfat ~ tanfat, data = data, subset = i)
  coef(mod)
}
set.seed(1)
b.obj <- boot(HSWRESTLER, statistic = caseresamp, R = 10000)
boot.ci(b.obj, type = "bca", index = 2, conf = 0.95)
confint(object = lm(hwfat ~ tanfat, data = HSWRESTLER), 
        parm = "tanfat", level = 0.95)

##################### R Code 10.46 #################################

hwfit <- function(data){
  coef(lm(data$hwfat ~ data$tanfat))
}
hwcase <- function(data, i){
  hwfit(data[i, ])
}
set.seed(1)
hwboot <- boot(HSWRESTLER, statistic = hwcase, R = 10000)
boot.ci(hwboot, type = "bca", index = 2, conf = 0.95)
confint(object = lm(hwfat ~ tanfat, data = HSWRESTLER), 
        parm = "tanfat", level = 0.95)

##################### R Code 10.47 #################################

hw.lm <- lm(hwfat ~ tanfat, data = HSWRESTLER)
hwfit <- function(data){
  coef(lm(data$hwfat ~ data$tanfat))
}
FAT <- data.frame(HSWRESTLER, RESIDr2 = resid(hw.lm)/
                    sqrt(1 - hatvalues(hw.lm)) - 
                    mean(resid(hw.lm)/sqrt(1 - hatvalues(hw.lm))), 
                  fit = fitted(hw.lm))
FAT[1:6, 7:11]
rresamp <- function(data, i){
  d <- data
  d$hwfat <- d$fit + d$RESIDr2[i]
  hwfit(d)
}
set.seed(1)
rboot <- boot(FAT, statistic = rresamp, R = 10000)
boot.ci(rboot, type = "bca", index = 2)
confint(hw.lm, parm = "tanfat")

##################### R Code 10.48 #################################

library(car)
set.seed(1)
summary(RbootC <- Boot(hw.lm, R = 10000, method = "case"))
confint(RbootC, level= 0.95, type = "bca", parm = "tanfat")

##################### R Code 10.49 #################################

library(car)
set.seed(1)
summary(RbootR <- Boot(hw.lm, R = 10000, method = "residual"))
confint(RbootR, level= 0.95, type = "bca", parm = "tanfat")
boot.ci(RbootR, level= 0.95, type = "bca", index = 2)

##################### Example 10.21 #################################

##################### R Code 10.50 #################################

Contr <- RATBP$mmHg[RATBP$group == "control"]
Treat <- RATBP$mmHg[RATBP$group == "treatment"]
mmHg <- RATBP$mmHg                          # combined values
mmHg
Contr
Treat
 
nx <- length(Contr)
ny <- length(Treat)
COMB <- t(combn(nx + ny, nx))
dim(COMB)
nn <- dim(COMB)[1]
tail(COMB)
 
mmHg[COMB[924, ]] 
mmHg[-COMB[924, ]]

##################### R Code 10.51 #################################

theta.obs <- mean(Contr) - mean(Treat)
theta.obs
theta.hat <- numeric(nn)
for(i in 1:nn){
  theta.hat[i] <- mean(mmHg[COMB[i, ]]) - mean(mmHg[-COMB[i, ]])
}
 
pvalue <- sum(theta.hat <= theta.obs)/choose(12, 6)
pvalue
# Or
mean(theta.hat <= theta.obs)

##################### R Code 10.52 #################################

library(coin)
oneway_test(mmHg ~ group, data = RATBP, alternative = "less", 
            distribution = "exact")

##################### R Code 10.53 #################################

B <- 10^4 - 1
theta.hatE <- numeric(B)
set.seed(1)
for(i in 1:B){
  index <- sample(12, 6, replace = FALSE)
  theta.hatE[i] <- mean(mmHg[index]) - mean(mmHg[-index])
}
pvalue <- (sum(theta.hatE <= theta.obs) + 1)/(B + 1)
pvalue

##################### R Code 10.54 #################################

library(boot)
blood.fun <- function(data, i){
  d <- data[i]
  M <- mean(d[1:6]) - mean(d[7:12])
  M
}
set.seed(13)
perm.obj <- boot(mmHg, statistic = blood.fun, R = B, sim = "permutation") 
perm.obj
pval.perm <- (sum(perm.obj$t <= perm.obj$t0) + 1)/(B + 1)
pval.perm

##################### R Code 10.55 #################################

##################### Figure 10.22 #################################

p <- ggplot(data = data.frame(x = theta.hat), aes(x = x)) + 
  geom_density(fill = "pink", alpha = 0.2) + 
  theme_bw() + 
  labs(x = "$\\hat{\\theta} = \\bar{z} - \\bar{y}$", y = "") + 
  geom_density(data =  data.frame(x = theta.hatE), aes(x = x), 
               color = "red", linetype = "dashed") + 
  geom_segment(aes(x = theta.obs, y = 0, xend = theta.obs, 
                   yend = 0.010), size = 0.1) +
  stat_function(fun = dnorm, args = list(mean = 0, 17.94905), 
                linetype = "dotted")  +
  geom_text(data = NULL, x = theta.obs, y = 0.011, 
            label = "$\\hat{\\theta}_{obs}$", size = 4)
p

