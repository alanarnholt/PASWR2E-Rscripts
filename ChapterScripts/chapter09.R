################### Chapter 09 Script
################### 7/24/15 V.1
library(PASWR2)
library(grid)
####################### Chapter 9 ###################


####################### Example 9.3 #################
 
ALPHA <- 1 - pnorm(2, 1, 2)
ALPHA
 
ALPHA <-pnorm(2, 1, 2, lower.tail = FALSE)
ALPHA
 
BETA <- pnorm(2, 4, 2)
BETA


####################### Figure 9.1 #################

curve(dnorm(x, 1, 2), -5, 10, axes = FALSE, ann = FALSE, n = 500)
curve(dnorm(x, 4, 2), -5, 10, add = TRUE, n = 500)
BETA <- pnorm(2, 4, 2)
ALPHA <- pnorm(2, 1, 2, lower = FALSE)
x <- seq(-5, qnorm(BETA, 4, 2), length = 100)
y <- dnorm(x, 4, 2)
xs <- c(-5, x, qnorm(BETA, 4, 2))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue3")
x <- seq(qnorm(1 - ALPHA, 1 , 2), 10, length = 100)
y <- dnorm(x, 1, 2)
xs <- c(qnorm(1 - ALPHA, 1, 2), x, 10)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue1")
# Retrace now
curve(dnorm(x, 1, 2), -5, 10, add = TRUE, n = 500, lwd = 2)
curve(dnorm(x, 4, 2), -5, 10, add = TRUE, n = 500, lwd = 2)
# Highlight x-axis
segments(-5, 0, 10, 0, lwd = 3)
#
segments(1, 0, 1, dnorm(1, 1, 2), lwd = 2)
segments(qnorm(1 - ALPHA, 1 ,2), 0, qnorm(1 - ALPHA, 1 ,2), dnorm(qnorm(1 - ALPHA, 1 ,2), 1, 2), lwd = 2)
segments(4, 0, 4, dnorm(4, 4, 2), lwd = 2)
###
arrows(-4, .20, 0, .03, length = .1)
mtext("$\\gil{P}(\\textrm{type II error}) = 0.1587$", side = 3, line = 0.3, at = - 3)
arrows(9, .20, 5, .03, length = .1)
mtext("$\\gil{P}(\\textrm{type I error}) = 0.3085$", side = 3, line = 0.3, at = 8)
### labels now
axis(side = 1, at = c(-5, 2, 1, 2, 4, 7, 10), line = -0.4)

####################### Example 9.4 #################

qexp(0.05, 2)

####################### Example 9.5 #################

pnorm(36, 40, 6/sqrt(9)) + 1 - pnorm(44, 40, 6/sqrt(9))


pnorm(38, 40, 6/sqrt(36)) + 1 - pnorm(42, 40, 6/sqrt(36))

####################### R Code 9.1 #################

####################### Figure 9.2 #################

mu <- seq(30,50,.01)
power9 <- 1-pnorm(44, mu, 6/sqrt(9)) + pnorm(36, mu, 6/sqrt(9))
power36 <- 1-pnorm(42, mu, 6/sqrt(36)) + pnorm(38, mu, 6/sqrt(36))
plot(mu, power9, type = "l", ylab = expression(Power(mu)),
    xlab = expression(mu), ylim=c(0, 1))
lines(mu, power36, type = "l", lty = "dashed")
arrows(32, 0.6, 34.2, 0.78, lwd = 2, length = 0.05)
arrows(32, 0.35 , 37, 0.78,  lwd = 2, length = 0.05)
arrows(40, 0.65 , 40, 0.06, lwd = 2, length = 0.05)
text(32,0.58, expression(n == 9))
text(32,0.33, expression(n == 36))
text(40, 0.70, expression(alpha == 0.045))


####################### Figure 9.2  (ggplot2) #################

library(ggplot2)
mu <- seq(30, 50, 0.01)
power9 <- 1 - pnorm(44, mu, 6/sqrt(9)) + pnorm(36, mu, 6/sqrt(9))
power36 <- 1 - pnorm(42, mu, 6/sqrt(36)) + pnorm(38, mu, 6/sqrt(36))
library(grid)
DF <- data.frame(x = mu, P9 = power9, P36 = power36)
p <- ggplot(data = DF, aes(x = mu, y = P9))
p + geom_line(color = "lightblue") + geom_line(aes(x = mu, y = P36), lty = "dashed", color = "blue") + theme_bw() + 
  annotate("segment", x = 32, xend = 37, y = 0.35, yend = 0.78, arrow = arrow(length = unit(0.2, "cm"))) + 
  annotate("segment", x = 32, xend = 34.2, y = 0.6, yend = 0.78, arrow = arrow(length = unit(0.2, "cm"))) + 
  annotate("segment", x = 40, xend = 40, y = 0.65, yend = 0.06, arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = 32, y = 0.33, label = "$n = 36$") + 
  annotate("text", x = 32, y = 0.58, label = "$n = 9$") +
  annotate("text", x = 40, y = 0.7, label ="$\\alpha = 0.045$") +
  labs(x = "$\\mu$", y = "$\\textrm{Power}(\\mu)$\n")


####################### Figure 9.3 #########################

curve(dnorm(x, 1, 1), -2, 5, axes = FALSE, ann = FALSE, n = 500)
curve(dnorm(x, 2, 1), -2, 5, add = TRUE, n = 500)
BETA <- pnorm(2.0364, 2, 1)
ALPHA <- pnorm(2.0364, 1, 1, lower = FALSE)
ALPHA
BETA
x <- seq(-2, qnorm(BETA, 2, 1), length = 200)
y <- dnorm(x, 2, 1)
xs <- c(-2, x, qnorm(BETA, 2, 1))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue3")
x <- seq(qnorm(1 - ALPHA, 1 , 1), 5, length = 200)
y <- dnorm(x, 1, 1)
xs <- c(qnorm(1 - ALPHA, 1, 1), x, 5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue1")
# Retrace now
curve(dnorm(x, 1, 1), -2, 5, add = TRUE, n = 500, lwd = 2)
curve(dnorm(x, 2, 1), -2, 5, add = TRUE, n = 500, lwd = 2)
# Highlight x-axis
segments(-2, 0, 5, 0, lwd = 3)
#
segments(1, 0, 1, dnorm(1, 1, 1), lwd = 2)
segments(qnorm(1 - ALPHA, 1 ,1), 0, qnorm(1 - ALPHA, 1 ,1), dnorm(qnorm(1 - ALPHA, 1 ,1), 2, 1), lwd = 2)
segments(2, 0, 2, dnorm(2, 2, 1), lwd = 2)
###
arrows(-1.5, .40, 0, .06, length = .1)
mtext("$\\gil{P}(\\textrm{type II error}) = 0.5145$", side = 3, line = 0, at = - 1)
arrows(4.5, .40, 3, .06, length = .1)
mtext("$\\gil{P}(\\textrm{type I error}) = 0.1500$", side = 3, line = 0, at = 4)
### labels now
axis(side = 1, at = c(-2, -1, 0, 1, 2.0364, 3, 4, 5), line = -0.4)


####################### Example 9.6 #################

ALPHA <- pnorm(1.3, 1, 1) - pnorm(1.1, 1, 1) + pnorm(2.4617, 1, 1, lower = FALSE)
ALPHA
BETA <- pnorm(1.1, 2, 1) + pnorm(2.4617, 2, 1) - pnorm(1.3, 2, 1)
BETA

####################### Figure 9.4 #################

curve(dnorm(x, 1, 1), -2, 5, axes = FALSE, ann = FALSE, n = 500, ylim = c(0, .5))
curve(dnorm(x, 2, 1), -2, 5, add = TRUE, n = 500)
x <- seq(-2, 1.1, length = 200)
y <- dnorm(x, 2, 1)
xs <- c(-2, x, 1.1)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue3")
##
x <- seq(1.1, 1.3, length = 100)
y <- dnorm(x, 1, 1)
xs <- c(1.1, x, 1.3)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue1")
##
x <- seq(1.3, 2.4617, length = 200)
y <- dnorm(x, 2, 1)
xs <- c(1.3, x, 2.4617)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue3")
##
x <- seq(2.4617, 5, length = 200)
y <- dnorm(x, 1, 1)
xs <- c(2.4617, x, 5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue1")
# Retrace now
curve(dnorm(x, 1, 1), -2, 5, add = TRUE, n = 500, lwd = 2)
curve(dnorm(x, 2, 1), -2, 5, add = TRUE, n = 500, lwd = 2)
# Highlight x-axis
segments(-2, 0, 5, 0, lwd = 3)
#
segments(1.1, 0, 1.1, dnorm(1.1, 1, 1), lwd = 2)
segments(1.3, 0, 1.3, dnorm(1.3, 1, 1), lwd = 2)
segments(2.4617, 0, 2.4617, dnorm(2.4617, 2, 1), lwd = 2)
###
arrows(-1.5, .50, 0, .06, length = .1)
arrows(-1.5, .50, 1.7, .39, length = .05)
mtext("$\\gil{P}(\\textrm{type II error}) = 0.6199$", side = 3, line = 0, at = - 1)
arrows(4.5, .5, 3, .06, length = .1)
arrows(4.5, .5, 1.3, .39, length= 0.05)
mtext("$\\gil{P}(\\textrm{type I error}) = 0.1500$", side = 3, line = 0, at = 4)
### labels now
axis(side = 1, at = c(-2, -1, 0, 1, 2, 3, 4, 5), line = -0.4)


####################### Table 9.6 #################

curve(dnorm(x, 0, 1), -3.5, 3.5, axes = FALSE, ann = FALSE, n = 500)
x <- seq(-3.5, qnorm(0.05), length = 200)
y <- dnorm(x, 0, 1)
xs <- c(-3.5, x, qnorm(.05))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(-3.5, 0, 3.5, 0, lwd = 3)
mtext("$\\Huge{z_{\\alpha}}$", side = 1, line = 0, at = qnorm(0.05))

 curve(dnorm(x, 0, 1), -3.5, 3.5, axes = FALSE, ann = FALSE, n = 500)
x <- seq(qnorm(0.95), 3.5, length = 200)
y <- dnorm(x, 0, 1)
xs <- c(qnorm(0.95), x, 3.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(-3.5, 0, 3.5, 0, lwd = 3)
mtext("$z_{1 - \\alpha}$", side = 1, line = 0, at = qnorm(0.95))

 curve(dnorm(x, 0, 1), -3.5, 3.5, axes = FALSE, ann = FALSE, n = 500)
x <- seq(qnorm(0.975), 3.5, length = 200)
y <- dnorm(x, 0, 1)
xs <- c(qnorm(0.975), x, 3.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
x <- seq(-3.5, qnorm(0.025), length = 200)
y <- dnorm(x, 0, 1)
xs <- c(-3.5, x, qnorm(.025))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(-3.5, 0, 3.5, 0, lwd = 3)
mtext("$z_{\\alpha/2}$", side = 1, line = 0, at = qnorm(0.025))
mtext("$z_{1 - \\alpha/2}$", side = 1, line = 0, at = qnorm(0.975))
 

####################### R Code 9.2 #################

z <- qnorm(0.95)                      # Critical Value
zobs <- ((56/30) - 1.8)/(2/sqrt(30))  # z_obs
pvalue <- pnorm(zobs, lower = FALSE)  # p-value
c(z = z, zobs = zobs, pvalue = pvalue)

####################### R Code 9.3 #################

beta3 <- pnorm(qnorm(0.95, 1.8, 2/sqrt(30)), 3, 2/sqrt(30))
power3 <- 1 - beta3
beta3
power3


#######################  Figure 9.5 #################

library(ggplot2)

p <- ggplot(data = data.frame(x = c(0.5, 4.3)), aes(x = x))
dnorm_func <- function(x){
  y <- dnorm(x, 3, 2/sqrt(30))
  y[x < qnorm(0.95, 1.8, 2/sqrt(30))] <- NA
  return(y)
}
dnorm_func1 <- function(x){
  y <- dnorm(x, 3, 2/sqrt(30))
  y[x >= qnorm(0.95, 1.8, 2/sqrt(30))] <- NA
  return(y)
}
p + stat_function(fun = dnorm_func, geom = "area", fill = "blue", 
    alpha = 0.2, n = 500) + 
    stat_function(fun = dnorm_func1, geom = "area", fill = "blue") + 
    geom_hline(yintercept = 0) + 
    stat_function(fun = dnorm, args = list(1.8, 2/ sqrt(30)), n = 500) + 
    stat_function(fun = dnorm, args = list(3, 2/sqrt(30)), n = 500) + 
    theme_bw() + 
    labs(x = "", y = "")
  

####################### Table 9.7 #################

curve(dt(x, 10), -3.5, 3.5, axes = FALSE, ann = FALSE, n = 500)
x <- seq(-3.5, qt(0.05, 10), length = 200)
y <- dt(x, 10)
xs <- c(-3.5, x, qt(.05, 10))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(-3.5, 0, 3.5, 0, lwd = 3)
mtext("$\\Huge{t_{\\alpha}}$", side = 1, line = 0, at = qt(0.05, 10))


curve(dt(x, 10), -3.5, 3.5, axes = FALSE, ann = FALSE, n = 500)
x <- seq(qt(0.95, 10), 3.5, length = 200)
y <- dt(x, 10)
xs <- c(qt(0.95, 10), x, 3.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(-3.5, 0, 3.5, 0, lwd = 3)
mtext("$t_{1 - \\alpha}$", side = 1, line = 0, at = qt(0.95, 10))

curve(dt(x, 10), -3.5, 3.5, axes = FALSE, ann = FALSE, n = 500)
x <- seq(qt(0.975, 10), 3.5, length = 200)
y <- dt(x, 10)
xs <- c(qt(0.975, 10), x, 3.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
x <- seq(-3.5, qt(0.025, 10), length = 200)
y <- dt(x, 10)
xs <- c(-3.5, x, qt(.025, 10))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(-3.5, 0, 3.5, 0, lwd = 3)
mtext("$t_{\\alpha/2}$", side = 1, line = 0, at = qt(0.025, 10))
mtext("$t_{1 - \\alpha/2}$", side = 1, line = 0, at = qt(0.975, 10))

####################### Example 9.8 #################

ct <- qt(0.975,24) 
S <- sqrt((600 - 25*4^2)/(25 - 1))
tobs <- (4 - 2.5)/(S/sqrt(25))
pvalue <- pt(tobs, 25 - 1, lower = FALSE)*2
c(ct = ct, S = S, tobs = tobs, pvalue = pvalue)
 
pt(qt(0.025, 24), 24, 3) + pt(qt(0.975, 24), 24, 3, lower = FALSE)
 
pf(qt(0.975,24)^2, 1, 24, 9, lower = FALSE)

 
power.t.test(n = 25, delta = 1.5, sd = 2.5, type = "one.sample", 
             strict = TRUE)



####################### Figure 9.6 #################

curve(dt(x, 24), -4, 8, axes = FALSE, ann = FALSE, n = 500)
curve(dt(x, 24, 3), -4, 8, add = TRUE, n = 500)
x <- seq(-4, qt(0.975, 24), length = 100)
y <- dt(x, 24, 3)
xs <- c(-4, x, qt(0.975, 24))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue1")
x <- seq(qt(0.975, 24), 8, length = 100)
y <- dt(x, 24, 3)
xs <- c(qt(0.975, 24), x, 8)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue3")
# Retrace now
curve(dt(x, 24), -4, 8, add = TRUE, n = 500, lwd = 2)
curve(dt(x, 24, 3), -4, 8, add = TRUE, n = 500, lwd = 2)
# Highlight x-axis
segments(-4, 0, 8, 0, lwd = 3)
#
segments(qt(0.975, 24), 0, qt(0.975, 24), dt(qt(0.975, 24), 24, 3), lwd = 2)
segments(qt(0.025, 24), 0, qt(0.025, 24), dt(qt(0.025, 24), 24), lwd = 2)
###
arrows(-3, .20, -2.1, .06, length = .1)
arrows(-3, .20, 2, .18, length = .1)
arrows(7, .20, 4, .1, length = .1)
mtext("Critical Values", 
      side = 3, line = -5.5, at = - 4)
mtext("$\\pow(\\mu_1=4)$", side = 3, line = -5.5, at = 7)
mtext("$t_{24}$", side = 3, line = 0, at = 0)
mtext("$t_{24;\\nct = 3}$", side = 3, line = -1, at = 3)
### labels now
axis(side = 1, at = -4:8, line = -0.4)

############################ R Code 9.5 #########################

set.seed(7)
SIMS <- 50000
n <- 25
tstar <- numeric(SIMS)
for(i in 1:SIMS){
  rs <- rnorm(n, 4, 2.5)
  tstar[i] <- (mean(rs) - 2.5) / (sd(rs) / sqrt(n)) 
}
power <- mean(tstar < qt(0.025, n - 1)) + 
  mean(tstar > qt(0.975, n - 1))
power

############################ R Code 9.6 #########################

DF <- data.frame(x = tstar)
x.dens <- density(tstar)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
p <- ggplot(data = DF) +
    geom_density(aes(x = x, y = ..density..), fill = "blue",
                 alpha = 0.2)
p + geom_area(data = subset(df.dens, x >= qt(0.975, 24) &
             x <= max(DF$x)), aes(x = x, y = y), fill = "blue",
             alpha = 1.0) +
   stat_function(fun = dt, args = list(df = 24)) +
   xlim(-4, 10) +
   geom_vline(xintercept = qt(c(0.025, 0.975), 24), lty = "dashed") +
   labs(x = "", y = "") +
   theme_bw()
  

############################ Example 9.9 #########################

ct <- qt(0.95, 14)  # critical value
Yield <- c(2.5, 3, 3.1, 4, 1.2, 5, 4.1, 3.9, 
           3.2, 3.3, 2.8, 4.1, 2.7, 2.9, 3.7)
xbar <- mean(Yield)
S <- sd(Yield)                    
tobs <- (xbar - 2) / (S / sqrt(15))
pvalue <- pt(tobs, 15 - 1, lower = FALSE)      # pvalue
pvalue
c(CriticalValue = ct, xbar = xbar, S = S, tobs = tobs)


############################ R Code 9.7 #########################
 
t.test(Yield, alternative = "greater", mu = 2)

############################ Example 9.10 #########################

sig <- sqrt(100^2 / 64 + 108^2 / 144)
cv <- qnorm(0.90, 0, sig)         # critical value
BETA <- pnorm(cv, 40, sig)
POWER <- 1 - BETA
POWER


############################ Figure 9.9 #########################

ggplot(data = STSCHOOL, aes(x = school, y = satisfaction, fill = school)) + 
  geom_boxplot() + theme_bw() 
ggplot(data = STSCHOOL, aes(sample = satisfaction, shape = school, 
                            color = school)) + 
  theme_bw() + stat_qq()
 

############################ Example 9.11 #########################

t.test(satisfaction ~ school, data = STSCHOOL, var.equal=TRUE)
 
gamma <- 10/sqrt(9^2/11 + 9^2/15)
gamma
power <- pt(qt(0.025, 24), 24, gamma) + pt(qt(0.975, 24), 24, gamma, lower = FALSE)
power
 
1 - pf(qt(0.975, 24)^2, 1, 24, gamma^2)


############################ Figure 9.10 #########################

ggplot(data = WATER, aes(x = source, y = sodium, fill = source)) + 
  geom_boxplot() + theme_bw() 
ggplot(data = WATER, aes(sample = sodium, shape = source, 
                            color = source)) + 
  theme_bw() + stat_qq()
 


############################ Example 9.12 #########################

t.test(sodium ~ source, data = WATER, var.equal = FALSE, alt = "less")

## ----c09edaBARLEY, echo = FALSE, dev = "tikz", crop = TRUE, fig.align = 'center', results = 'hide', fig.height = 5, fig.width = 5, out.width='0.65\\linewidth', warning = FALSE----
yieldMor32 <- barley$yield[barley$year == "1932" & barley$site =="Morris"]
yieldCro32 <- barley$yield[barley$year == "1932" & barley$site =="Crookston"]
d <- yieldMor32 - yieldCro32
eda(d)
 

############################ R Code 9.8 #########################

ct <- qt(0.95, 9)  # Critical Value
ct
yieldMor32 <- barley$yield[barley$year == "1932" & barley$site =="Morris"]
yieldCro32 <- barley$yield[barley$year == "1932" & barley$site =="Crookston"]
d <- yieldMor32 - yieldCro32
t.test(d, alternative = "greater") # t-test on d

############################ R Code 9.9 #########################

yieldMor32 <- barley$yield[barley$year == "1932" & barley$site =="Morris"]
yieldCro32 <- barley$yield[barley$year == "1932" & barley$site =="Crookston"]
t.test(yieldMor32, yieldCro32, paired = TRUE, alternative = "greater")
DF <- stack(data.frame(yieldMor32, yieldCro32))
head(DF) # show first 6 rows of DF
t.test(values ~ ind, data = DF, paired = TRUE, alternative = "less")


############################ Table 9.12 #########################

curve(dchisq(x, 5), 0, 20, axes = FALSE, ann = FALSE, n = 500)
x <- seq(0, qchisq(0.05, 5), length = 200)
y <- dchisq(x, 5)
xs <- c(0, x, qchisq(.05, 5))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(0, 0, 20, 0, lwd = 3)
mtext("$\\chi^2_{\\alpha}$", side = 1, line = 0, at = qchisq(0.05, 5))

curve(dchisq(x, 5), 0, 20, axes = FALSE, ann = FALSE, n = 500)
x <- seq(qchisq(0.95, 5), 20, length = 200)
y <- dchisq(x, 5)
xs <- c(qchisq(0.95, 5), x, 20)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(0, 0, 20, 0, lwd = 3)
mtext("$\\chi^2_{1 - \\alpha}$", side = 1, line = 0, at = qchisq(0.95, 5))

curve(dchisq(x, 5), 0, 20, axes = FALSE, ann = FALSE, n = 500)
x <- seq(qchisq(0.975, 5), 20, length = 200)
y <- dchisq(x, 5)
xs <- c(qchisq(0.975, 5), x, 20)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
x <- seq(0, qchisq(0.025, 5), length = 200)
y <- dchisq(x, 5)
xs <- c(0, x, qchisq(.025, 5))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(0, 0, 20, 0, lwd = 3)
mtext("$\\chi^2_{\\alpha/2}$", side = 1, line = 0, at = qchisq(0.025, 5))
mtext("$\\chi^2_{1 - \\alpha/2}$", side = 1, line = 0, at = qchisq(0.975, 5))


############################ Figure 9.12 #########################

diameter <- WASHER$diameter
eda(diameter)
rm(diameter)
 

############################ Example 9.14 #########################

cv <- qchisq(0.95,19)            # Critical Value
s2 <- var(WASHER$diameter)
n <- sum(!is.na(WASHER$diameter))
n
Chi2Obs <- (n - 1)*s2 / 0.004     # Standardized Test Statistic's Value
Chi2Obs
pvalue <- pchisq(Chi2Obs, n-1, lower = FALSE)      
c(CriticalValue = cv, Chi2Obs = Chi2Obs, pvalue = pvalue)


############################ Table 9.14 #########################

curve(df(x, 5, 10), 0, 6, axes = FALSE, ann = FALSE, n = 500) # comment
x <- seq(0, qf(0.05, 5, 10), length = 200)
y <- df(x, 5, 10)
xs <- c(0, x, qf(.05, 5, 10))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(0, 0, 6, 0, lwd = 3)
mtext("$f_{\\alpha}$", side = 1, line = 0, at = qf(0.05, 5, 10))

curve(df(x, 5, 10), 0, 6, axes = FALSE, ann = FALSE, n = 500) # comment
x <- seq(qf(0.95, 5, 10), 6, length = 200)
y <- df(x, 5, 10)
xs <- c(qf(0.95, 5, 10), x, 6)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(0, 0, 6, 0, lwd = 3)
mtext("$f_{1 - \\alpha}$", side = 1, line = 0, at = qf(0.95, 5, 10))

curve(df(x, 5, 10), 0, 6, axes = FALSE, ann = FALSE, n = 500) # comment
x <- seq(qf(0.975, 5, 10), 6, length = 200)
y <- df(x, 5, 10)
xs <- c(qf(0.975, 5, 10), x, 6)
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
x <- seq(0, qf(0.025, 5, 10), length = 200)
y <- df(x, 5, 10)
xs <- c(0, x, qf(.025, 5, 10))
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightblue")
segments(0, 0, 6, 0, lwd = 3)
mtext("$f_{\\alpha/2}$", side = 1, line = 0, at = qf(0.025, 5, 10))
mtext("$f_{1 - \\alpha/2}$", side = 1, line = 0, at = qf(0.975, 5, 10))

############################ Example 9.15 #########################

shapiro.test(BAC$Y)


cv <-  qf(0.05, 9, 9)      # Critical Value
X <- BAC$X
Y <- BAC$Y
VX <- var(X)
VY <- var(Y)
RV <- VX / VY              # Standardized Test Statistic's Value
pvalue <- pf(RV, 9, 9)     # P-Value
c(CriticalValue = cv, VarianceX = VX, VarianceY = VY, 
  TestStat = RV, Pvalue = pvalue)
 
var.test(X, Y, alternative = "less")
# OR using a formula
DF <- stack(data.frame(X, Y))
var.test(values ~ ind, data = DF, alternative = "less")
rm(X, Y)  # Clean up


############################ Figure 9.13 #########################

X <- BAC$X
Y <- BAC$Y
eda(X)
eda(Y)
rm(X, Y)


############################ Example 9.16 #########################

probs <- dbinom(0:500, 500, 0.2)
pvalue <- sum(probs[probs <= dbinom(90, 500, 0.2)])
pvalue
binom.test(x = 90, n = 500, p = 0.2)

############################ R Code 9.11 #########################
Y <- 90
n <- 500
p <- Y / n
PI <- 0.2
zobs <- (p - PI) / sqrt((PI * (1 - PI))/n)  # No c.c.
pval <- 2 * pnorm(zobs)
zobsC <- (p - PI + 1/(2 * n) ) / sqrt((PI * (1 - PI))/n) # Yes c.c.
pvalC <- 2 * pnorm(zobsC)
c(ZNCC = zobs, PvalueNCC = pval, ZYCC = zobsC, PvalueYCC = pvalC)

############################ R Code 9.12 #########################

prop.test(x = 90, n = 500, p = 0.2, correct = FALSE)
prop.test(x = 90, n = 500, p = 0.2, correct = TRUE)

############################ R Code 9.13 #########################

p <- dhyper(0:6, 9, 7, 6) # Probabilities for the 7 possible 2X2 tables
p
pobs <- dhyper(1, 9, 7, 6)
pval <- sum(p[p<=pobs])
pval

############################ R Code 9.14 #########################

JV <- matrix(data = c(1, 5, 8, 2), nrow = 2)
dimnames(JV) <- list(Youth = c("Delinquent", "Non-delinquent"), Glasses = c("Yes", "No"))
JV
fisher.test(JV)
 

############################ R Code 9.15 #########################

HA <- matrix(c(104, 189, 10933, 10845), nrow = 2) # Table
dimnames(HA) <- list(Treatment=c("Aspirin","Placebo"), 
                     Outcome=c("Heart attack","No heart attack"))
HA
pval <- phyper(104, 104 + 10933, 189 + 10845, 104 + 189)  # x, m, n, k
pval
fisher.test(HA, alternative = "less") 

############################ R Code 9.16 #########################

prop.test(x = c(104, 189), n = c(11037, 11034), correct = FALSE, 
          alternative = "less")
zobsNCC <- prop.test(x = c(104, 189), n = c(11037, 11034), 
                     correct = FALSE, alternative = "less")$stat^0.5
names(zobsNCC) <- NULL
zobsNCC  # Z obs no continuity correction
prop.test(x = c(104, 189), n = c(11037, 11034), correct = TRUE, 
          alternative = "less")
zobsYCC <- prop.test(x = c(104, 189), n = c(11037, 11034), 
                     correct = TRUE, alternative = "less")$stat^0.5
names(zobsYCC) <- NULL
zobsYCC

