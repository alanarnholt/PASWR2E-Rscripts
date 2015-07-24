################### Chapter 07 Script
################### 7/24/15 V.1
library(PASWR2)
########################### Chapter 7 ###########################

########################### Figure 7.1 ##########################

plot(0:10, 0:10, type = "n", axes = FALSE, xlab = "", ylab = "")
symbols(x = c(2.5, 2.5, 2.5) , y = c(7.5, 7.5, 7.5), circles = c(1, 2, 3) ,add = TRUE)
symbols(x = c(7.5, 7.5, 7.5) , y = c(7.5, 7.5, 7.5), circles = c(1, 2, 3) ,add = TRUE)
symbols(x = c(2.5, 2.5, 2.5) , y = c(2.5, 2.5, 2.5), circles = c(1, 2, 3) ,add = TRUE)
symbols(x = c(7.5,7.5, 7.5) , y = c(2.5,2.5,2.5), circles = c(1,2,3) ,add = TRUE)
points(x=c(2.4, 2.6, 2.3, 2.5, 2.7, 2.4, 2.6), 
       y=c(7.3, 7.3, 7.5, 7.5, 7.5, 7.7, 7.7), pch = 19)
points(x=c(7.9, 8.1, 7.8, 8, 8.2, 7.9, 8.1) , 
       y=c(8.3, 8.3, 8.5, 8.5, 8.5, 8.7, 8.7), pch = 19)
points(x=c(1.4, 3.6, 2.3, 1.5, 2.7, 2.4, 3.6), 
       y=c(2.2, 1.8, 3.1, 2.7, 2.5, 1.5, 3.2), pch = 19)
points(x=c(8, 8.5, 9.1, 8.2, 8.4, 8.6, 9) , 
       y=c(2.8, 2.1, 2.7, 2.9, 3.3, 3.1, 3.5), pch = 19)
text(2.5, 10, "Low Variance, Low Bias")
text(7.5, 10, "Low Variance, High Bias")
text(2.5, 5, "High Variance, Low Bias")
text(7.5, 5, "High Variance, High Bias")



########################### R Code 7.1 ##########################

########################### Figure 7.2 ##########################


f <- function(x){sqrt(2/(x - 1))*gamma(x/2)/gamma((x - 1)/2)}
p <- ggplot(data.frame(x = c(2, 50)), aes(x = x))
p + stat_function(fun = f) +
   labs(x = "n", y = expression(frac(sqrt(2)*phantom(0)*Gamma*
      bgroup("(",frac(n, 2),")"), sqrt(n - 1)*phantom(0)*Gamma*
      bgroup("(",frac(n - 1, 2),")")))) +
   geom_hline(yintercept = 1, lty = "dashed") +
   theme_bw()

########################### Example 7.5 ##########################

Mu <- sqrt((0.125 - 0.081675)/0.0001)
Mu


########################### Figure 7.3 ##########################

curve(dnorm(x, 100, 15), 40, 210, ylim = c(0, .03), axes = FALSE, xlab = "", ylab = "")
curve(dnorm(x, 125, 20), add = TRUE)
segments(40, 0, 210, 0, lwd = 2)
segments(100, 0, 100, dnorm(100, 100, 15), lty = "dashed")
segments(125, 0, 125, dnorm(125, 125, 20), lty = "dashed")
arrows(100, .0025, 125, .0025, length = .08, code = 3)
text(112.5, .0025+.002, "Bias")
arrows(160, .027, 111, .023, length = 0.08)
text(160, .027 + .001, "Distribution of $\\hat{\\mu}_1$")
arrows(160, .021, 135, .019, length = 0.08)
text(160, .021 + .001, "Distribution of $\\hat{\\mu}_2$")
mtext("$0.99\\mu$", side = 1, line = 0, at = 100)
mtext("$\\mu$", side = 1, line = 0, at = 125)
 

########################### Example 7.9 ##########################

stem1 <- c(1.7, 2.8, 3.2, 3.4, 5.3, 5.9, 6.2, 7.2, 8.3, 9.3)
stem2 <- c(1.7, 2.8, 3.2, 3.4, 5.3, 5.9, 6.2, 7.2, 83, 9.3)
c(mean(stem1), sqrt(var(stem1)))
c(mean(stem2), sqrt(var(stem2)))
c(median(stem1), mad(stem1, constant = 1))
c(median(stem2), mad(stem2, constant = 1))
median(abs(stem1 - median(stem1)))
median(abs(stem2 - median(stem2)))

########################### R Code 7.2 ##########################

########################### Figure 7.4 ##########################

loglike <- function(p){40*log(2) + 220*log(p) + 158*log(1 - p)}
negloglike <- function(p){(-1)*(40*log(2) + 220*log(p) + 158*log(1 - p))}
p1 <- ggplot(data = data.frame(x =c(0, 1)), aes(x = x))
p1 + stat_function(fun = loglike, n = 200) +
 labs(x = "p",
      y = expression(textstyle(ln)~~L(p*"|"*bold(x))) ) +
 geom_vline(xintercept = 0.58, lty = "dashed")
 

########################### R Code 7.3 ###########################

nlm(f = negloglike, p = 0.01)$estimate
optimize(f = loglike, interval = c(0, 1), maximum = TRUE)$maximum

########################### Example 7.18 ##########################

eggs <- ROACHEGGS$eggs
mean(eggs)

loglike <- function(PI){(sum(eggs)*log(PI)+ sum(1 - eggs)*log(1 - PI))}
negloglike <- function(PI){(-1)*(sum(eggs)*log(PI) + 
                                   sum(1 - eggs)*log(1 - PI))}
nlm(f = negloglike, p = 0.2)


########################### R Code 7.4 ##########################

########################### Figure 7.5 ##########################

p <- ggplot(data.frame(x = c(0, 1)), aes(x = x))
p + stat_function(fun = loglike, n = 200) +
 labs(x = expression(pi),
      y = expression(textstyle(ln)~~L(pi*"|"*bold(x))) ) +
 geom_vline(xintercept = mean(eggs), lty = "dashed")


########################### R Code 7.5 ##########################

optimize(f=loglike, interval=c(0, 1), maximum = TRUE)
 

########################### Example 7.19 ##########################

set.seed(23)
pihat1 <- sum(rbinom(1000, 3, 0.5))/(1000 * 3)
pihat1
 
set.seed(23)
pihat2 <- mean(rbinom(1000, 3, 0.5))/3
pihat2

########################### R Code 7.6 ##########################

set.seed(99)
lambdahat <- mean(rpois(20000, 5))
lambdahat

########################### R Code 7.7 ##########################

set.seed(6)
mle <- max(runif(n = 1000, min = 0, max = 2))
mle


########################### Figure 7.6 ##########################

xs <- seq(0.3, 5, length = 500)
fxs <- 1 /xs
plot(xs, fxs, type = "l", lty = "dashed", xlim = c(0, 5), 
     ylim = c(0, 2), axes = FALSE, xlab = "", ylab = "")
xss <- subset(xs, xs > 2)
fxss <- 1/xss
lines(xss, fxss, lwd = 3)
segments(mle, 0, mle, 1/mle, lty = "dashed")
arrows(0, 0, 5, 0, length = 0.05, code = 2)
arrows(0, 0, 0, 2, length = 0.05, code = 2)
segments(0, 0, mle, 0, lwd = 3)
axis(side = 1)
mtext("$\\theta$", side = 1, line = 2, at = 2.5)
mtext("$L(\\theta|\\text{\\bo{x}})$", side = 2, line = 0.5)
arrows(3, 1, mle, 0.52, length = 0.05)
text(3, 1.1, "max($x_i$) = 1.9993")

########################### R Code 7.8 ##########################

sigma <- 1
mu1 <- 4
n <- 1000
set.seed(33)
x <- rnorm(n = 1000, mean = mu1, sd = sigma)
loglikemu <- function(mu){-n/2*log(2*pi) - n/2*log(sigma^2) - 
                          (sum(x^2) - 2*mu*sum(x) + n*mu^2)}
negloglikemu <- function(mu){(-1)*(-n/2*log(2*pi) - n/2*log(sigma^2) - 
                             (sum(x^2) - 2*mu*sum(x) + n*mu^2))}
loglikesig2 <- function(sig2){-n/2*log(2*pi) - n/2*log(sig2) - 
                              (sum((x - mu1)^2))/(2*sig2)}
negloglikesig2 <- function(sig2){(-1)*(-n/2*log(2*pi) - n/2*log(sig2) - 
                                 (sum((x - mu1)^2))/(2*sig2))}
EM1 <- nlm(f = negloglikemu, p = 2)$estimate
EM1
EM2 <- optimize(loglikemu, interval = c(2, 6), maximum = TRUE)$maximum
EM2
ES1 <- nlm(f = negloglikesig2, p = 0.5)$estimate
ES1
ES2 <- optimize(f = loglikesig2, interval = c(0.5, 2.5), 
                maximum = TRUE)$maximum
ES2

########################### Figure 7.7 ##########################

p <- ggplot(data.frame(x = c(2, 6.05)), aes(x = x))
p + stat_function(fun = loglikemu, n = 200) +
 labs(x = expression(mu),
      y = expression(textstyle(ln)~~L(mu*"|"*bold(x),sigma==1)) ) +
 geom_vline(xintercept = EM1, lty = "dashed")

p <- ggplot(data.frame(x = c(0.5, 2.45)), aes(x = x))
p + stat_function(fun = loglikesig2, n = 200) +
 labs(x = expression(sigma^2),
      y = expression(textstyle(ln)~~L(sigma^2*"|"*bold(x),mu==4)) ) +
 geom_vline(xintercept = ES1, lty = "dashed")

########################### R Code 7.10 ##########################

set.seed(11)
n <- 500
x <- rnorm(n, 2, 1)
Xbar <- mean(x)
S2u <- sum((x - mean(x))^2/n)
c(Xbar, S2u)

########################### R Code 7.11  ##########################

negloglike <- function(p){ (n/2)*log(2*pi) + (n/2)*log(2*p[2]) + 
                             (1/(2*p[2]))*sum((x - p[1])^2) }
nlm(f = negloglike, p = c(3, 2))$estimate

