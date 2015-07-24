################### Chapter 04 Script
################### 7/24/15 V.1
library(PASWR2)
##################### Chapter 4   ###############################

##################### Example 4.1 ###############################

##################### R Code 4.1 ###############################

Watts <- c(40, 60, 75, 100, 120)
n <- length(Watts)
meanWatts <- (1/n)*sum(Watts)
varWatts<- (1/n)*sum((Watts - meanWatts)^2)
ans <- c(meanWatts, varWatts)
ans

 
##################### R Code 4.2 ###############################

opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), pty = "s")
x <- 0:8
px <- dbinom(x, 8, 0.3)
plot(x, px, type = "h", xlab = "x", ylab="P(X = x)",
    main = "PDF of X~Bin(8, 0.3)")
xs <- rep(0:8, round(dbinom(0:8, 8, .3)*100000, 0))
plot(ecdf(xs), main = "CDF of X~Bin(8, 0.3)",
    ylab = expression(P(X<=x)), xlab = "x")
par(opar)


##################### Example 4.2 ###############################

##################### Figure 4.2  ###############################

x <- 0:10
n <- 10
DF <- stack(list('$\\pi = 0.2$' = dbinom(x, n, .2), '$\\pi = 0.5$' = dbinom(x, n, .5), '$\\pi = 0.8$' = dbinom(x, n, 0.8)))
names(DF) <- c("px", "pi")
DF$x <- x
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = px)) + facet_grid(pi~.) + 
  geom_linerange(aes(x = x, ymin = 0, ymax = px), size = 1) + 
  geom_point(color = "skyblue3", size = 3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_text(vjust = 0.2)) +
  labs(y = "$\\gil{P}(X=x)$", x = "$x$", title = "$X \\sim Bin(n = 10, \\pi)$\n")
theme_set(previous_theme) # Restore original theme

##################### R Code 4.3  ###############################

bino.gen <- function (samples = 10000, n = 20, pi = 0.5)
{
   values <- sample(c(0, 1), samples * n, replace = TRUE,
                    prob = c(1 - pi, pi))
   value.mat <- matrix(values, ncol = n)
   Successes <- apply(value.mat, 1, sum)
   a1 <- round((table(Successes)/samples), 3)
   b1 <- round(dbinom(0:n, n, pi), 3)
   names(b1) <- 0:n
   hist(Successes, breaks = c((-0.5 + 0):(n + 0.5)), freq = FALSE,
       ylab = "", col = 13, ylim = c(0, max(a1, b1)),
       main = " Theoretical Values Superimposed
        Over Histogram of Simulated Values")
   x <- 0:n
   fx <- dbinom(x, n, pi)
   lines(x, fx, type = "h")
   lines(x, fx, type = "p", pch = 16)
   list(simulated.distribution = a1, theoretical.distribution = b1)
}

##################### R Code 4.4  ###############################

##################### Figure 4.3  ###############################

set.seed(31)
bino.gen(samples = 1000, n = 5, pi = 0.5)
  

##################### R Code 4.5  ###############################

set.seed(123)
x <- rbinom(1000, 5, .5)
table(x)/1000     # Empirical distribution

##################### Example 4.3  ###############################

sum(dbinom(6:10, 10, 0.33))          # P(X >= 6)
1 - pbinom(5, 10, 0.33)              # 1 - P(X <= 5)
pbinom(5, 10, 0.33, lower = FALSE)   # P(X >= 6)
1 - sum(dbinom(5:0, 10, 0.33))       # 1 - P(X <= 5)
 

##################### R Code 4.6 ###############################

##################### Figure 4.4 ###############################

opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), pty = "s")
x <- 0:8
px <- dpois(x, 1)
plot(x, px, type = "h", xlab = "x", ylab="P(X = x)",
    main = "PDF of X ~ Pois(1)")
xs <- rep(0:8, round(dpois(0:8, 1)*100000, 0))
plot(ecdf(xs), main = "CDF of X ~ Pois(1)",
    ylab = expression(P(X <=x)), xlab = "x")
par(opar)


##################### Figure 4.5  ###############################

library(ggplot2)
x <- 0:20
DF <- stack(list('$\\lambda=4$' = dpois(x, 4), '$\\lambda=7$' = dpois(x, 7), '$\\lambda=10$' = dpois(x, 10) ))
names(DF) <- c("px", "lambda")
DF$x <- x
DF$lambda <- factor(DF$lambda, levels =c('$\\lambda=4$','$\\lambda=7$', '$\\lambda=10$'))
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = px)) + facet_grid(lambda~.) + 
  geom_linerange(aes(x = x, ymin = 0, ymax = px), size = 1) + 
  geom_point(color = "skyblue3", size = 3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_text(vjust = 0.2)) +
  labs(y = "$\\gil{P}(X=x)$", x = "$x$", title = "$X \\sim Pois(\\lambda)$\n")
theme_set(previous_theme) # Restore original theme
 

##################### R Code 4.7  ###############################

library(PASWR2)

L1 <- SOCCER$goals[1:228]
L2 <- SOCCER$goals[2:229]
L3 <- SOCCER$goals[3:230]
L4 <- SOCCER$goals[4:231]
L5 <- SOCCER$goals[5:232]
LAG <- cbind(L1, L2, L3, L4, L5)
# or more succinctly
LAG <- sapply(1:5, function(x){SOCCER$goals[x:(x+227)]})
round(cor(LAG), 3)

xtabs(~ goals, data = SOCCER)
table(SOCCER$goals)

mean(SOCCER$goals, na.rm = TRUE)
var(SOCCER$goals, na.rm = TRUE)        


##################### R Code 4.8  ###############################

OBS <- xtabs(~goals, data = SOCCER)
Empir <- round(OBS/sum(OBS), 3)
TheoP <- round(dpois(0:(length(OBS) - 1), mean(SOCCER$goals, na.rm=TRUE)), 3)
EXP <- round(TheoP*232, 0)
ANS <- cbind(OBS, EXP, Empir, TheoP)
ANS

##################### Example 4.6  ###############################

1 - ppois(q = 7, lambda = 4)               # P(X > 7|4)
ppois(q = 7, lambda = 4, lower = FALSE)    # P(X > 7|4)

ppois(q = 3, lambda = 8)                   # P(X <= 3|8)


##################### Example 4.7  ###############################

dpois(x = 0, lambda = 2)

ppois(q = 4, lambda = 2)

ppois(q = 5, lambda = 120)


##################### Example 4.8  ###############################

x <- 0:8
round(dbinom(x, 100, 0.04), 3)
round(dpois(x, 4), 3)


##################### R Code 4.9  ###############################

##################### Figure 4.6  ###############################

opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), pty = "s")
x <- 0:15
px <- dgeom(x, .3)
plot(x, px, type = "h", xlab = "x", ylab="P(X = x)",
     main = "PDF of X ~ Geom(0.3)")
xs <- rep(0:15, round(dgeom(0:15, 0.3)*100000, 0))
plot(ecdf(xs), main = "CDF of X ~ Geom(0.3)",
     ylab = expression(P(X <=x)), xlab = "x")
par(opar)


##################### Figure 4.7  ###############################

x <- 0:10
DF <- stack(list('$\\pi=0.5$' = dgeom(x, 0.5), '$\\pi=0.4$' = dgeom(x, 0.4), '$\\pi=0.3$' = dgeom(x, 0.3) ))
names(DF) <- c("px", "pi")
DF
DF$x <- x
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = px)) + facet_grid(pi~.) + 
  geom_linerange(aes(x = x, ymin = 0, ymax = px), size = 1) + 
  geom_point(color = "skyblue3", size = 3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_text(vjust = 0.2)) +
  labs(y = "$\\gil{P}(X=x)$", x = "$x$", title = "$X \\sim \\geo(\\pi)$\n")
theme_set(previous_theme) # Restore original theme
 


##################### Example 4.9  ###############################

dgeom(3, 0.2)  # P(X = 3|0.2)


##################### R Code 4.10  ###############################

##################### Figure 4.8   ###############################

opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), pty = "s")
x <- 0:20
r <- 6
px <- dnbinom(x, r, .5)
plot(x, px, type = "h", xlab = "x", ylab="P(X = x)", main = "PDF of X ~ NB(6,0.5)")
xs <- rep(0:20, round(dnbinom(0:20, r, 0.5)*100000, 0))
plot(ecdf(xs), main = "CDF of X ~ NB(6, 0.5)", ylab = "P(X <= x)", xlab = "x")
par(opar)


##################### Figure 4.9  ###############################

x <- 0:20
y1 <- dnbinom(x, 1, .4)
y2 <- dnbinom(x, 1, .6)
y3 <- dnbinom(x, 3, .4)
y4 <- dnbinom(x, 3, .6)
y5 <- dnbinom(x, 5, .4)
y6 <- dnbinom(x, 5, .6)
DF <- data.frame(px = c(y1, y2, y3, y4, y5, y6), x = x, r = 0, pi = 0)
DF$r[1:42] = "$r = 1$"
DF$r[43:84] = "$r = 3$"
DF$r[85:126] = "$r = 5$"
DF$pi[c(1:21, 43:63, 85:105)] = "$\\pi = 0.4$"
DF$pi[c(22:42, 64:84, 106:126)] = "$\\pi = 0.6$"
#
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = px)) + facet_grid(pi~r) + 
  geom_linerange(aes(x = x, ymin = 0, ymax = px), size = 1) + 
  geom_point(color = "skyblue3", size = 3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_text(vjust = 0.2)) +
  labs(y = "$\\gil{P}(X=x)$", x = "$x$", title = "$X \\sim NB(r, \\pi)$\n")
theme_set(previous_theme) # Restore original theme
 

##################### Example 4.10  ###############################

dnbinom(2, 4, 0.9)  # P(X = 2 | 4, 0.9)


##################### Example 4.11  ###############################

dhyper(25, 147, 3, 25) # P(X = 25 | m = 147, n = 3, k = 25) 

##################### R Code 4.11  ###############################

##################### Figure 4.10  ###############################

opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), pty = "s")
x <- 0:10
m <- 15
n <- 10
k <- 10
px <- dhyper(x, m, n, k)
plot(x, px, type = "h", xlab = "x", ylab="P(X = x)",
     main = "PDF of X ~ Hyper(15, 10, 10)")
xs <- rep(x, round(dhyper(x, m, n, k)*10000000, 0))
plot(ecdf(xs), main = "CDF of X ~ Hyper(15, 10, 10)",
     ylab = expression(P(X <=x)), xlab = "x")
par(opar)


##################### Figure 4.11  ###############################

x <- 0:20
n <- 10
y1 <- dhyper(x, 10, n, 20)
y2 <- dhyper(x, 10, n, 5)
y3 <- dhyper(x, 40, n, 20)
y4 <- dhyper(x, 40, n, 5)
y5 <- dhyper(x, 60, n, 20)
y6 <- dhyper(x, 60, n, 5)
DF <- data.frame(px = c(y1, y2, y3, y4, y5, y6), x = x, m = 0, k = 0)
DF$m[1:42] = "$m = 10$"
DF$m[43:84] = "$m = 40$"
DF$m[85:126] = "$m = 60$"
DF$k[c(1:21, 43:63, 85:105)] = "$k = 20$"
DF$k[c(22:42, 64:84, 106:126)] = "$k = 5$"
DF$k <- factor(DF$k, levels =c('$k = 5$','$k = 20$'))
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = px)) + facet_grid(k~m) + 
  geom_linerange(aes(x = x, ymin = 0, ymax = px), size = 1) + 
  geom_point(color = "skyblue3", size = 3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_text(vjust = 0.2)) +
  labs(y = "$\\gil{P}(X=x)$", x = "$x$", title = "$X \\sim \\hyper(m, 10, k)$\n")
theme_set(previous_theme) # Restore original theme

##################### Figure 4.12  ###############################


x <- 0:5
n <- 5
DF <- stack(list('$\\pi = 0.5$' = dbinom(x, n, .5), '$\\pi = 0.8$' = dbinom(x, n, .8), '$\\pi = \\frac{6}{7}$' = dbinom(x, n, 6/7)))
names(DF) <- c("px", "pi")
DF
DF$x <- x
DF
DF$pi <- factor(DF$pi, levels =c('$\\pi = 0.5$', '$\\pi = 0.8$', '$\\pi = \\frac{6}{7}$'))
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = px)) + facet_grid(.~pi) + 
  geom_linerange(aes(x = x, ymin = 0, ymax = px), size = 1) + 
  geom_point(color = "skyblue3", size = 3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_text(vjust = 0.2)) +
  labs(y = "$\\gil{P}(X=x)$", x = "$x$", title = "$X \\sim Bin(n = 5, \\pi)$\n")
theme_set(previous_theme) # Restore original theme
 


##################### Figure 4.13  ###############################

x <- seq(-1, 11, length = 500)
y1 <- dunif(x, 0, 8)
y2 <- dunif(x, 4, 8)
y3 <- punif(x, 0, 8)
y4 <- punif(x, 4, 8)
DF <- data.frame(fx = c(y1, y2, y3, y4))
DF$dist[1:500] = "$Unif(0, 8)$"
DF$dist[501:1000] = "$Unif(4, 8)$"
DF$dist[1001:1500] = "$Unif(0, 8)$"
DF$dist[1501:2000] = "$Unif(4, 8)$"
DF$type[1:1000] = "$f(x)$"
DF$type[1001:2000] = "$F(x)$"
DF$x = x
#
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) + geom_area(fill = "skyblue3") + facet_grid(type~dist) + 
  labs(x = "$x$", y = "") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
theme_set(previous_theme) # Restore original theme  

##################### R Code 4.12  ###############################

set.seed(13)
MV <- (pi/6)*mean(runif(10000,3,5)^3)
MV

##################### R Code 4.13  ###############################

set.seed(33)
U <- runif(10000)
X <- sqrt((2 - sqrt(4 - 3 * U)))
SM <- mean(X)
SV <- var(X)
TM <- 84/135
TV <- 116/2025
PE <- c(PercentErrorMean  = abs(SM - TM)/TM*100, 
        PercentErrorVariance = abs(SV - TV)/TV*100)
ANS <- c(SimMean = SM, TheMean = TM, SimVar = SV, TheoVar = TV)
ANS
PE

##################### R Code 4.14  ###############################

f <- function(x){(4/3)*x*(2 - x^2)}
ex <- function(x){x*f(x)}
ex2 <- function(x){x^2*f(x)}
EX <- integrate(ex, 0, 1)
EX2 <- integrate(ex2, 0, 1)
VX <- EX2$value - EX$value^2
c(EX$value, EX2$value, VX)


##################### Figure 4.14  ###############################

x <- seq(-1, 4, length = 500)
y1 <- dexp(x, 3)
y2 <- dexp(x, 1)
y3 <- pexp(x, 3)
y4 <- pexp(x, 1)
DF <- data.frame(fx = c(y1, y2, y3, y4))
DF$dist[1:500] = "$Exp(\\lambda = 3)$"
DF$dist[501:1000] = "$Exp(\\lambda = 1)$"
DF$dist[1001:1500] = "$Exp(\\lambda = 3)$"
DF$dist[1501:2000] = "$Exp(\\lambda = 1)$"
DF$type[1:1000] = "$f(x)$"
DF$type[1001:2000] = "$F(x)$"
DF$x = x
#
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) + geom_area(fill = "skyblue3") + facet_grid(type~dist) + 
  labs(x = "$x$", y = "") 
theme_set(previous_theme) # Restore original theme  

##################### Example 4.17  ###############################

pexp(12, 1/8) - pexp(3, 1/8)
 
f1 <- function(x){(1/8) * exp(-x/8)}         # define f1
integrate(f1, lower = 3, upper = 12)$value   # integrate f1                                
 
qexp(0.95, 1/8)
 
pexp(25, 1/8, lower = FALSE)/pexp(10, 1/8, lower = FALSE)
# OR
1 - pexp(15, 1/8)
# OR
pexp(15, 1/8, lower = FALSE)

##################### Example 4.18  ###############################

inter.times <- with(data = SOCCER, cgt[2:575] - cgt[1:574])
MEAN <- mean(inter.times)
SD <- sd(inter.times)
c(MEAN = MEAN, SD = SD)
 
rate <- (575/232)*(1/90)            # rate = lambda*t
nit <- sum(!is.na(inter.times))     # number of inter.times
OBS <- xtabs(~cut(inter.times, breaks = c(seq(0, 130, 10), 310)))
EmpiP <- round(OBS/nit, 3)
TheoP <- round(c((pexp(seq(10, 130, 10),rate) - 
                  pexp(seq(0, 120, 10), rate)), 
                  (1 - pexp(130, rate))), 3)
EXP <-round(TheoP*nit, 0)
ANS <-cbind(OBS, EXP, EmpiP, TheoP)
ANS


##################### R Code 4.15  ###############################

##################### Figure 4.15  ###############################


DF <- data.frame(x = inter.times)
previous_theme <- theme_set(theme_bw())  # set black and white theme
p <- ggplot(data = DF, aes(x = x)) +
 geom_histogram(aes(y = ..density..), fill = "pink", binwidth = 10,
                color = "black") +
labs(x = "Time Between Goals (minutes)", y = "")
p + stat_function(fun = dexp, arg = list(rate = rate), size = 1)
theme_set(previous_theme) # Restore original theme
 


##################### Figure 4.16  ###############################

x <- seq(-1, 11, length = 500)
y1 <- dgamma(x, 2, 1)
y2 <- dgamma(x, 2, 2)
y3 <- dgamma(x, 2, 4)
y4 <- dgamma(x, 3, 1)
y5 <- dgamma(x, 3, 2)
y6 <- dgamma(x, 3, 4)
y7 <- dgamma(x, 4, 1)
y8 <- dgamma(x, 4, 2)
y9 <- dgamma(x, 4, 4)
DF <- data.frame(fx = c(y1, y2, y3, y4, y5, y6, y7, y8, y9))
DF$a[1:1500] = "$\\alpha = 2$"
DF$a[1501:3000] = "$\\alpha = 3$"
DF$a[3001:4500] = "$\\alpha = 4$"
DF$l[1:500] = "$\\lambda = 1$"
DF$l[501:1000] = "$\\lambda = 2$"
DF$l[1001:1500] = "$\\lambda = 4$"
DF$l[1501:2000] = "$\\lambda = 1$"
DF$l[2001:2500] = "$\\lambda = 2$"
DF$l[2501:3000] = "$\\lambda = 4$"
DF$l[3001:3500] = "$\\lambda = 1$"
DF$l[3501:4000] = "$\\lambda = 2$"
DF$l[4001:4500] = "$\\lambda = 4$"
DF$x = x
#
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) + geom_area(fill = "skyblue3") + facet_grid(l~a) + 
  labs(x = "$x$", y = "$f(x)$", title = "$X \\sim \\Gamma(\\alpha, \\lambda)$\n") + 
  theme(axis.title.y=element_text(vjust = 0.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
theme_set(previous_theme) # Restore original theme  

##################### Example 4.20  ###############################

1 - ppois(4, 6)
# OR
ppois(4, 6, lower = FALSE)
 
1 - pgamma(1, 2, 3)
gam23 <- function(x){9*x*exp(-3*x)}
integrate(gam23, 1, Inf)$value               


##################### Figure 4.17  ###############################

h <- function(x, alpha = 1, beta = 1){
  (alpha*x^(alpha -1))/(beta^alpha)
}
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), pty = "m")
curve(h(x, alpha = 1.5), 0, 5, lty = "dashed", ylab = "$h(t)$", xlab = "$t$", 
      main = "Hazard Function, $\\beta = 1$", lwd = 2)
curve(h(x, alpha = 0.5), 0, 5, add = TRUE, lty = "dotted", lwd = 2)
curve(h(x, alpha = 1), 0, 5, add = TRUE, lty = "solid", lwd = 2)
leglabels <- c("$\\alpha = 0.5$", "$\\alpha = 1.0$", "$\\alpha = 1.5$")
leglty <- c("dotted", "solid", "dashed")
legend("topleft", legend = leglabels, lty = leglty, lwd = 2)
### pdfs now
curve(dweibull(x, 1.5, 1), 0, 5, ylab = "$f(t)$", xlab = "$t$", lty = "dashed",
      main = "Corresponding PDFs", ylim = c(0, 1.5), lwd = 2)
curve(dweibull(x, 0.5, 1), add = TRUE, lty= "dotted", lwd = 2)
curve(dweibull(x, 1, 1), add = TRUE, lty = "solid", lwd = 2)
leglabels <- c("$\\alpha = 0.5$", "$\\alpha = 1.0$", "$\\alpha = 1.5$")
leglty <- c("dotted", "solid", "dashed")
legend("topright", legend = leglabels, lty = leglty, lwd = 2)
par(opar)

##################### R Code 4.16  ###############################

f <- function(x){2000 - 0.1*exp(-2*x)}
K <- 1 / (integrate(f, lower = 0, upper = 5)$value)
K
 
 
ansB <- K*integrate(f, lower = 0, upper = 1)$value
ansB 
 
et <- function(x){x*K*f(x)}
ET <- integrate(et, lower = 0, upper = 5)$value
ET
 
 
et2 <- function(x){x^2*K*f(x)}
ET2 <- integrate(et2, lower = 0, upper = 5)$value
VX <- ET2 - ET^2
SX <- sqrt(VX)
SX
  
ansB
dbinom(0, 5, ansB)
 
fr <- function(x){2000*x + 0.05*exp(-2*x) - 0.05 - 0.05/K}
ansE <- uniroot(fr, c(0, 5))$root
ansE

##################### R Code 4.17  ###############################

h <- function(x){0.0001*(2000 - 0.1*exp(-2*x)) /
   (1 - 0.0001*(2000*x + 0.05*exp(-2*x) - 0.05))
}
# Base graphics
curve(h, from = 0, to = 4.99, n= 1000, xlab = "year", ylab = "h(year)")
# ggplot2 now
p <- ggplot(data.frame(x = c(0, 4.99)), aes(x = x))
p + stat_function(fun = h) + labs(x = "year", y = "h(year)")
 



##################### Figure 4.19  ###############################

x <- seq(0, 7, length = 500)
y1 <- dweibull(x, 1, 1)
y2 <- dweibull(x, 1, 2)
y3 <- dweibull(x, 1, 3)
y4 <- dweibull(x, 2, 1)
y5 <- dweibull(x, 2, 2)
y6 <- dweibull(x, 2, 3)
y7 <- dweibull(x, 3, 1)
y8 <- dweibull(x, 3, 2)
y9 <- dweibull(x, 3, 3)
DF <- data.frame(fx = c(y1, y2, y3, y4, y5, y6, y7, y8, y9))
DF$a[1:1500] = "$\\alpha = 1$"
DF$a[1501:3000] = "$\\alpha = 2$"
DF$a[3001:4500] = "$\\alpha = 3$"
DF$b[1:500] = "$\\beta = 1$"
DF$b[501:1000] = "$\\beta = 2$"
DF$b[1001:1500] = "$\\beta = 3$"
DF$b[1501:2000] = "$\\beta = 1$"
DF$b[2001:2500] = "$\\beta = 2$"
DF$b[2501:3000] = "$\\beta = 3$"
DF$b[3001:3500] = "$\\beta = 1$"
DF$b[3501:4000] = "$\\beta = 2$"
DF$b[4001:4500] = "$\\beta = 3$"
DF$x = x
#
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) + geom_area(fill = "skyblue3") + facet_grid(b~a) + 
  labs(x = "$x$", y = "$f(x)$", title = "$X \\sim \\weib(\\alpha, \\beta)$\n") + 
  theme(axis.title.y=element_text(vjust = 0.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
theme_set(previous_theme) # Restore original theme  

##################### Example 4.22  ###############################

1 - pweibull(8, 2, 8)
# OR
pweibull(8, 2, 8, lower = FALSE)
8*gamma(3/2)
# OR
4*sqrt(pi)


##################### Figure 4.20  ###############################

x <- seq(0+0.02, 1-0.02, length = 500)
y1 <- dbeta(x, 1/3, 1/3)
y2 <- dbeta(x, 1/3, 1)
y3 <- dbeta(x, 1/3, 3)
y4 <- dbeta(x, 1, 1/3)
y5 <- dbeta(x, 1, 1)
y6 <- dbeta(x, 1, 3)
y7 <- dbeta(x, 3, 1/3)
y8 <- dbeta(x, 3, 1)
y9 <- dbeta(x, 3, 3)
DF <- data.frame(fx = c(y1, y2, y3, y4, y5, y6, y7, y8, y9))
DF$a[1:1500] = "$\\alpha = \\frac{1}{3}$"
DF$a[1501:3000] = "$\\alpha = 1$"
DF$a[3001:4500] = "$\\alpha = 3$"
DF$b[1:500] = "$\\beta = \\frac{1}{3}$"
DF$b[501:1000] = "$\\beta = 1$"
DF$b[1001:1500] = "$\\beta = 3$"
DF$b[1501:2000] = "$\\beta = \\frac{1}{3}$"
DF$b[2001:2500] = "$\\beta = 1$"
DF$b[2501:3000] = "$\\beta = 3$"
DF$b[3001:3500] = "$\\beta = \\frac{1}{3}$"
DF$b[3501:4000] = "$\\beta = 1$"
DF$b[4001:4500] = "$\\beta = 3$"
DF$x = x
#
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) + geom_area(fill = "skyblue3") + facet_grid(b~a) + 
  labs(x = "$x$", y = "$f(x)$", title = "$X \\sim \\beta(\\alpha, \\beta)$\n") + 
  theme(axis.title.y=element_text(vjust = 0.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
theme_set(previous_theme) # Restore original theme  

##################### R Code 4.18  ###############################

pbeta(0.8, 4, 2, lower = FALSE)
b42 <- function(x){(gamma(6)/(gamma(4)*gamma(2)))*x^3*(1 - x)}
integrate(b42, lower = 0.8, upper = 1)$value

##################### Example 4.24  ###############################

GB <- function(x){(1/8) * (gamma(5)/(gamma(2) * gamma(3))) * 
                    ((x - 8)/8) * ((16 - x)/8)^2}
integrate(GB, lower = 8, upper = 10)$value
 
A <- 8
B <- 16
x <- 10
ans <- pbeta((x - A)/(B - A), 2, 3)
ans


##################### Figure 4.21  ###############################

par(mfrow = c(1, 3))
# First
curve(dnorm(x, 0, 1/2), -3.3/2, 3.3/2, axes = FALSE, ann = FALSE, 
      xlim = c(-6.6, 6.6), ylim =c(0, .8))
segments(-3.3/2,0,3.3/2,0,lwd = 2)
segments(0, 0, 0, dnorm(0, 0, 1/2), lty="dotted")
mtext(side = 1, line = 0, "$\\mu$")
# Second
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE, 
      xlim = c(-6.6, 6.6), ylim =c(0, .8))
segments(-3.3,0,3.3,0,lwd = 2)
segments(0, 0, 0, dnorm(0, 0, 1), lty="dotted")
mtext(side = 1, line = 0, "$\\mu$")
# Third
curve(dnorm(x, 0, 2), -3.3*2, 3.3*2, axes = FALSE, ann = FALSE, 
      xlim = c(-6.6, 6.6), ylim =c(0, .8))
segments(-3.3*2,0,3.3*2,0,lwd = 2)
segments(0, 0, 0, dnorm(0, 0, 2), lty="dotted")
segments(2, 0, 2, dnorm(2, 0, 2), lty="dotted")
mtext(side = 1, line = 0, "$\\mu$")
arrows(0,.05,2,.05,length=.05, code = 3)
text(1, .09, "$\\sigma$")
#
par(mfrow=c(1,1))


##################### Figure 4.22  ###############################

par(mfrow = c(2, 3))
# First
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE)
x <- seq(-1, 1.5, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-1, x, 1.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(-1, 0, -1, dnorm(-1), lwd = 2)
segments(1.5, 0, 1.5, dnorm(1.5), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE, add = TRUE, lwd = 2, xpd = NA)
text(1.6,.3, "\\Large$f(x)$")
segments(4.3, .19, 5.3, .19, lwd = 5, xpd = NA)
segments(4.3, .21, 5.3, .21, lwd = 5, xpd = NA)
axis(1, at = c(-1, 1.5), labels = c("\\Large$a$", "\\Large$b$"), tick = FALSE, padj = -1)
mtext(text="$\\int_a^b f(x)\\,dx$", side = 1, line = 2.5)
mtext(text="$\\Updownarrow$", side = 1, line = 5.5)
mtext(text="$\\gil{P}(a \\leq X \\leq b)$", side = 3, line = 0)
# Second
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "")
x <- seq(-3.3, 1.5, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-3.3, x, 1.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(1.5, 0, 1.5, dnorm(1.5), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "", add = TRUE, lwd = 2, xpd = NA)
text(1.6,.3, "\\Large$f(x)$")
segments(4.3, .20, 5.3, .20, lwd = 5, xpd = NA)
axis(1, at = 1.5, labels = "\\Large$b$", tick = FALSE, padj = -1)
mtext(text="$\\int_{-\\infty}^b f(x)\\,dx$", side = 1, line = 2.5)
mtext(text="$\\Updownarrow$", side = 1, line = 5.5)
mtext(text="$\\gil{P}(X \\leq b)$", side = 3, line = 0)
# Third
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "")
x <- seq(-3.3, -1, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-3.3, x, -1)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(-1, 0, -1, dnorm(-1), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "", add = TRUE, lwd = 2)
text(1.6,.3, "\\Large$f(x)$")
axis(1, at = -1, labels = "\\Large$a$", tick = FALSE, padj = -1)
mtext(text="$\\int_{-\\infty}^a f(x)\\,dx$", side = 1, line = 2.5)
mtext(text="$\\Updownarrow$", side = 1, line = 5.5)
mtext(text="$\\gil{P}(X \\leq a)$", side = 3, line = 0)
#
# Fourth
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE)
x <- seq(-1, 1.5, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-1, x, 1.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(-1, 0, -1, dnorm(-1), lwd = 2)
segments(1.5, 0, 1.5, dnorm(1.5), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE, add = TRUE, lwd = 2, xpd = NA)
text(1.6,.3, "\\Large$f(z)$")
segments(4.3, .19, 5.3, .19, lwd = 5, xpd = NA)
segments(4.3, .21, 5.3, .21, lwd = 5, xpd = NA)
axis(1, at = c(-1, 1.5), labels = c("\\Large$\\frac{a-\\mu}{\\sigma}$", "\\Large$\\frac{b-\\mu}{\\sigma}$"), tick = FALSE, padj = -1)
mtext(text="$\\int_{\\frac{b-\\mu}{\\sigma}}^{\\frac{b-\\mu}{\\sigma}} f(z)\\,dz$", side = 1, line = 3)
mtext(text="$\\gil{P}(\\frac{a-\\mu}{\\sigma} \\leq Z \\leq \\frac{b-\\mu}{\\sigma})$", side = 3, line = 0)
# Fifth
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "")
x <- seq(-3.3, 1.5, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-3.3, x, 1.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(1.5, 0, 1.5, dnorm(1.5), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "", add = TRUE, lwd = 2, xpd = NA)
text(1.6,.3, "\\Large$f(z)$")
segments(4.3, .20, 5.3, .20, lwd = 5, xpd = NA)
axis(1, at = 1.5, labels = "\\Large$\\frac{b-\\mu}{\\sigma}$", tick = FALSE, padj = -1)
mtext(text="$\\int_{-\\infty}^{\\frac{b-\\mu}{\\sigma}} f(z)\\,dz$", side = 1, line = 3)
mtext(text="$\\gil{P}(Z \\leq \\frac{b-\\mu}{\\sigma})$", side = 3, line = 0)
# Sixth
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "")
x <- seq(-3.3, -1, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-3.3, x, -1)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(-1, 0, -1, dnorm(-1), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "", add = TRUE, lwd = 2)
text(1.6,.3, "\\Large$f(z)$")
axis(1, at = -1, labels = "\\Large$\\frac{a-\\mu}{\\sigma}$", tick = FALSE, padj = -1)
mtext(text="$\\int_{-\\infty}^{\\frac{a-\\mu}{\\sigma}} f(z)\\,dz$", side = 1, line = 3)
mtext(text="$\\gil{P}(Z \\leq \\frac{a-\\mu}{\\sigma})$", side = 3, line = 0)
#
par(mfrow = c(1, 1))


##################### Figure 4.23 ###############################

par(mfrow = c(2, 3))
# First
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE)
x <- seq(-1, 1.5, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-1, x, 1.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(-1, 0, -1, dnorm(-1), lwd = 2)
segments(1.5, 0, 1.5, dnorm(1.5), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE, add = TRUE, lwd = 2, xpd = NA)
text(1.6,.3, "\\Large$f(x)$")
segments(4.3, .19, 5.3, .19, lwd = 5, xpd = NA)
segments(4.3, .21, 5.3, .21, lwd = 5, xpd = NA)
axis(1, at = c(-1, 1.5), labels = c("\\Large$90$", "\\Large$115$"), tick = FALSE, padj = -1)
mtext(text="$\\int_{90}^{115} f(x)\\,dx$", side = 1, line = 2.5)
mtext(text="$\\Updownarrow$", side = 1, line = 5.5)
mtext(text="$\\gil{P}(90 \\leq X \\leq 115)$", side = 3, line = 0)
# Second
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "")
x <- seq(-3.3, 1.5, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-3.3, x, 1.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(1.5, 0, 1.5, dnorm(1.5), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "", add = TRUE, lwd = 2, xpd = NA)
text(1.6,.3, "\\Large$f(x)$")
segments(4.3, .20, 5.3, .20, lwd = 5, xpd = NA)
axis(1, at = 1.5, labels = "\\Large$115$", tick = FALSE, padj = -1)
mtext(text="$\\int_{-\\infty}^{115} f(x)\\,dx$", side = 1, line = 2.5)
mtext(text="$\\Updownarrow$", side = 1, line = 5.5)
mtext(text="$\\gil{P}(X \\leq 115)$", side = 3, line = 0)
# Third
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "")
x <- seq(-3.3, -1, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-3.3, x, -1)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(-1, 0, -1, dnorm(-1), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "", add = TRUE, lwd = 2)
text(1.6,.3, "\\Large$f(x)$")
axis(1, at = -1, labels = "\\Large$90$", tick = FALSE, padj = -1)
mtext(text="$\\int_{-\\infty}^{90} f(x)\\,dx$", side = 1, line = 2.5)
mtext(text="$\\Updownarrow$", side = 1, line = 5.5)
mtext(text="$\\gil{P}(X \\leq 90)$", side = 3, line = 0)
# Fourth
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE)
x <- seq(-1, 1.5, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-1, x, 1.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(-1, 0, -1, dnorm(-1), lwd = 2)
segments(1.5, 0, 1.5, dnorm(1.5), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE, add = TRUE, lwd = 2, xpd = NA)
text(1.6,.3, "\\Large$f(z)$")
segments(4.3, .19, 5.3, .19, lwd = 5, xpd = NA)
segments(4.3, .21, 5.3, .21, lwd = 5, xpd = NA)
axis(1, at = c(-1, 1.5), labels = c("\\Large$-1$", "\\Large$1.5$"), tick = FALSE, padj = -1)
mtext(text="$\\int_{-1}^{1.5} f(z)\\,dz$", side = 1, line = 2.5)
mtext(text="$\\gil{P}(\\frac{90 - 100}{10} \\leq Z \\leq \\frac{115 - 100}{10})$", side = 3, line = 0)
# Fifth
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "")
x <- seq(-3.3, 1.5, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-3.3, x, 1.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(1.5, 0, 1.5, dnorm(1.5), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "", add = TRUE, lwd = 2, xpd = NA)
text(1.6,.3, "\\Large$f(z)$")
segments(4.3, .20, 5.3, .20, lwd = 5, xpd = NA)
axis(1, at = 1.5, labels = "\\Large$1.5$", tick = FALSE, padj = -1)
mtext(text="$\\int_{-\\infty}^{1.5} f(z)\\,dz$", side = 1, line = 2.5)
mtext(text="$\\gil{P}(Z \\leq \\frac{115 - 100}{10})$", side = 3, line = 0)
# Sixth
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "")
x <- seq(-3.3, -1, length = 500)
y <- dnorm(x, 0, 1)
xs <- c(-3.3, x, -1)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.3,0, 3.3,0, lwd = 3)
segments(-1, 0, -1, dnorm(-1), lwd = 2)
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, xlab ="", ylab = "", add = TRUE, lwd = 2)
text(1.6,.3, "\\Large$f(z)$")
axis(1, at = -1, labels = "\\Large$-1$", tick = FALSE, padj = -1)
mtext(text="$\\int_{-\\infty}^{-1} f(z)\\,dz$", side = 1, line = 2.5)
mtext(text="$\\gil{P}(Z \\leq \\frac{90 - 100}{10})$", side = 3, line = 0)
#
par(mfrow = c(1, 1))

##################### Example 4.25  ###############################

pnorm(115, 100, 10) - pnorm(90, 100, 10)
 
qnorm(.90, 100, 10)
 
qnorm(0.10 + pnorm(105, 100, 10), 100, 10)
 

##################### Example 4.26  ###############################

p <- pnorm(0.7, 0.5, 0.1)
p
 
sum(dbinom(19:20, 20, p))
# OR
pbinom(18, 20, p, lower = FALSE)

##################### R Code 4.19  ###############################

n <- length(SCORE$scores)
X <- (1:n - 1/2)/n
Xs <- qnorm(X)
Ys <- sort(SCORE$scores)
plot(Xs, Ys)
quantile(Xs, c(0.25, 0.75))
quantile(Ys, c(0.25, 0.75))


##################### Figure 4.24  ###############################

qqnorm(SCORE$scores)
qqline(SCORE$scores)

#####################  R Code 4.20 ###############################
 
qqmath(~scores, data = SCORE)
ggplot(data = SCORE, aes(sample = scores)) + stat_qq()
 

##################### Figure 4.26  ###############################

set.seed(7)
DF <- stack(list("Normal" = rnorm(500, 0, 1), "Short Tailed" = runif(500, -1, 1), "Long Tailed" = rt(500, 1)))
names(DF) <- c("values", "Shape")
previous_theme <- theme_set(theme_bw()) # set black and white theme
ggplot(data = DF, aes(sample = values, shape = Shape, color = Shape)) + 
  stat_qq(size = 1) + geom_abline(slope = 1, lty = "dashed") + xlim(-3, 3) + 
  ylim(-3, 3) + scale_color_grey() + 
  guides(colour = guide_legend(override.aes = list(size=4)))
set.seed(8)
DF <- stack(list("Normal" = rnorm(500, 0, 1), "Skew Right" = rexp(500, 1), "Skew Left" = (-1)*rexp(500, 1)))
names(DF) <- c("values", "Shape")
ggplot(data = DF, aes(sample = values, shape = Shape, color = Shape)) + 
  stat_qq(size = 1) + geom_abline(slope = 1, lty = "dashed") + xlim(-3, 3) + 
  ylim(-3, 3) + scale_color_grey() + 
  guides(colour = guide_legend(override.aes = list(size=4)))
theme_set(previous_theme) # Restore original theme 


##################### Figure 4.27  ###############################

set.seed(3)
ntester(SCORE$scores)


##################### Figure 4.28  ###############################

with(data = SCORE, eda(scores))

