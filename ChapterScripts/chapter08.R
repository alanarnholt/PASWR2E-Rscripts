################### Chapter 08 Script
################### 7/24/15 V.1
library(PASWR2)
library(binom)
######################## Chapter 8 ############################## 

######################## Figure 8.1 #############################

curve(dnorm(x, 0, 1), -3.5, 3.5, axes = FALSE, ann = FALSE, n = 500)
alpha <- 0.05
x <- seq(-3.5, qnorm(alpha/2), length = 100)
y <- dnorm(x, 0, 1)
xs <- c(-3.5, x, qnorm(alpha/2))
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
x <- seq(qnorm(1 - alpha/2), 3.5, length = 100)
y <- dnorm(x, 0, 1)
xs <- c(qnorm(1 - alpha/2), x, 3.5)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(-3.5, 0, 3.5, 0, lwd = 3)
curve(dnorm(x, 0, 1), -3.5, 3.5, axes = FALSE, ann = FALSE, add = TRUE, lwd = 2)
segments(qnorm(alpha/2), 0, qnorm(alpha/2), dnorm(qnorm(alpha/2)), lwd = 2)
segments(qnorm(1 - alpha/2), 0, qnorm(1 - alpha/2), dnorm(qnorm(1 - alpha/2)), lwd = 2)
arrows(-3, 0.08, -2.5, 0.02, length = .06)
arrows(3, 0.08, 2.5, 0.02, length = .06)
text(-3, 0.1, "$\\alpha/2$")
text(3, 0.1, "$\\alpha/2$")
text(0, 0.17, "$1 - \\alpha$")
mtext("$z_{\\alpha/2}$", side = 1, line = 0, at = qnorm(alpha/2))
mtext("$z_{1-\\alpha/2}$", side = 1, line = 0, at = qnorm(1-alpha/2))
 

########################### R Code 8.1 ##########################

norsim <- function(sims = 100, n = 36, mu = 100, sigma = 18, 
                   conf.level = 0.95){
  alpha <- 1 - conf.level
  CL <- conf.level * 100
  ll <- numeric(sims)
  ul <- numeric(sims)
  for (i in 1:sims){
    xbar <- mean(rnorm(n , mu, sigma))
    ll[i] <- xbar - qnorm(1 - alpha/2)*sigma/sqrt(n)
    ul[i] <- xbar + qnorm(1 - alpha/2)*sigma/sqrt(n)
  }
  notin <- sum((ll > mu) + (ul < mu))
  percentage <- round((notin/sims) * 100, 2)
  SCL <- 100 - percentage
  plot(ll, type = "n", ylim = c(min(ll), max(ul)), xlab = " ", 
       ylab = " ")
  for (i in 1:sims) {
    low <- ll[i]
    high <- ul[i]
    if (low < mu & high > mu) {
      segments(i, low, i, high)
    }
    else if (low > mu & high > mu) {
      segments(i, low, i, high, col = "red", lwd = 5)
    }
    else {
      segments(i, low, i, high, col = "blue", lwd = 5)
    }
  }
  abline(h = mu)
  cat(SCL, "\b% of the random confidence intervals contain Mu =", mu, "\b.", "\n")
}

########################### R Code 8.2 ##########################

set.seed(10)
norsim(sims = 100, n = 36, mu = 100, sigma = 18, conf.level = 0.95)
 

########################### Example 8.3 ##########################

with(data = GROCERY, qqnorm(amount))
with(data = GROCERY, qqline(amount))

 

xbar <- mean(GROCERY$amount)
z <- qnorm(0.985)
CI1 <- xbar + c(-1, 1) * z * sqrt(900)/sqrt(30)
CI1
# Or using the z.test function
CI2 <-  z.test(GROCERY$amount, sigma.x = sqrt(900), n.x = 30, 
               conf.level = .97)$conf
CI2
 

z.test(GROCERY$amount, sigma.x = sqrt(900), n.x = 30, 
               conf.level = .99, alternative = "greater")$conf

########################### R Code 8.3 ##########################

nsize(b = .02, sigma = 0.1, conf.level = .95, type = "mu")

########################### R Code 8.4 ##########################

nsize(b = 1, sigma = 5, conf.level = .95, type = "mu")

########################### R Code 8.5 ##########################

f <- 0      # f = number of failures
p <- 0      # p = probability
s <- 100    # s = number of successes
while(p < 0.95){
   f <- f + 1
   p <- pnbinom(f, s, 0.03)
} 
ans <- c(f + s, p)  # f + s = Containers
names(ans) <- c("Containers", "Probability")
ans

########################### R Code 8.6 ##########################

roots <- polyroot(z = c(100^2, -6.078731, 0.0009))
roots
Re(roots)
r1 <- Re(roots[1]) # Real root
r2 <- Re(roots[2]) # Real root
c(r1, r2)
(100 - 0.03*r1)/sqrt(0.0291*r1)  # This does not equal -1.645
(100 - 0.03*r2)/sqrt(0.0291*r2)  # This does equal -1.645

########################### R Code 8.7 ##########################

n <- 0    # Number of containers
p <- 0    # Probability
while(p < 0.95) {
   n <- n + 1
   p <- 1 - pbinom(99, n, 0.03)
}
ans <- c(n, p)
names(ans) <- c("Containers", "Probability")
ans

########################### R Code 8.8 ##########################

n <- 12
xbar <- 61.9/n
s <- ((450 -n*(xbar)^2)/(n - 1))^0.5
ct <- qt(0.95, n - 1)
c(n, xbar, s, ct)
CI <- c(xbar + c(-1,1)*ct*s/sqrt(n))
CI
# Or using tsum.test()
tsum.test(mean.x = xbar, s.x = s, n.x = n, conf.level =0.90)$conf

########################### R Code 8.9 ##########################

xbar <- mean(HOUSE$price)
CT <- qt(0.975, 13)     # critical t value
ST <- sd(HOUSE$price)   # standard deviation
xbar + c(-1, 1)*CT*ST/sqrt(14)
# Second approach
t.test(HOUSE$price, conf.level = 0.95)$conf


########################### Figure 8.4 ##########################

with(data = HOUSE, qqnorm(price))
with(data = HOUSE, qqline(price))
 

########################### Example 8.9 ##########################

z <- qnorm(0.975)
pe <- 4 - 4.41
sigma <- 3
nx <- 15
ny <- 22
pe + c(-1, 1)*z*sigma*sqrt(1/nx + 1/ny)
# Second approach using zsum.test()
CI <- zsum.test(mean.x = 4, mean.y = 4.41, sigma.x = 3, sigma.y = 3, n.x = 15, n.y = 22, conf.level = 0.95)$conf
CI

########################### R Code 8.10 ##########################

########################### Figure 8.5  ##########################

ggplot(data = APPLE, aes(sample = hardness, shape = location)) +
 stat_qq() +
 theme_bw()
 

########################### R Code 8.11 ##########################

MEANS <- tapply(APPLE$hardness, APPLE$location, mean)
MEANS
pe <- MEANS[1] - MEANS[2]
pe + c(-1, 1)*qnorm(0.975)*1.5*sqrt(1/17 + 1/17)
# Using z.test
freshH <- subset(APPLE, select = hardness, 
                 subset = location =="fresh", drop = TRUE)
wareH <- subset(APPLE, select = hardness, 
                subset = location =="warehouse", drop = TRUE)
CI <- z.test(x = freshH, y = wareH, sigma.x = 1.5, sigma.y = 1.5)$conf
CI

########################### Example 8.11 ##########################

nx <- 50
ny <- 46
xbar <- 420/nx
ybar <- 405/ny
sigmax <- 4.5
sigmay <- 6
pe <- xbar - ybar  # point estimate
z <- qnorm(0.985)  # z_0.985
pe + c(-1, 1)*z*sqrt(sigmax^2/nx + sigmay^2/ny)  # CI
# Or using zsum.test
CI <- zsum.test(mean.x = xbar, mean.y = ybar, sigma.x = sigmax, 
       sigma.y = sigmay, n.x = nx, n.y = ny, conf.level = 0.97)$conf
CI


########################### Figure 8.6  ##########################

ggplot(data = CALCULUS, aes(sample = score, shape = calculus)) + stat_qq() + theme_bw()

########################### R Code 8.12 ##########################

MEANS <- tapply(CALCULUS$score, CALCULUS$calculus, mean)
MEANS
pe <- MEANS[2] - MEANS[1]
z <- qnorm(0.975)
pe + c(-1, 1)*z*sqrt(25/18 + 144/18)
# Using z.test
ScoreYesCalc <- subset(CALCULUS, select = score, subset = calculus == "Yes", drop = TRUE)
ScoreNoCalc <- subset(CALCULUS, select = score, subset = calculus == "No", drop = TRUE)
CI <- z.test(x = ScoreYesCalc, y = ScoreNoCalc, sigma.x = sqrt(25), sigma.y = sqrt(144), conf.level = 0.95)$conf
CI

########################### Example 8.13 ##########################

nx <- 15
ny <- 11
xbar <- 53/nx
ybar <- 77/ny
s2x <- (222 - nx*xbar^2)/(nx - 1)
s2y <- (560 -ny*ybar^2)/(ny - 1)
sp2 <- ((nx - 1)*s2x + (ny - 1)*s2y)/(nx + ny - 2)
sp <- sqrt(sp2)
ct <- qt(0.975, nx + ny - 2)
pe <- xbar - ybar
c(xbar, ybar, pe, s2x, s2y, sp2, sp, ct)
pe + c(-1, 1)*ct*sp*sqrt(1/nx + 1/ny)
# Or using tsum.test
CI <- tsum.test(mean.x = xbar, mean.y = ybar, s.x = sqrt(s2x), s.y = sqrt(s2y), n.x = nx, n.y = ny, var.equal = TRUE)$conf
CI

########################### Example 8.14 ##########################

CI <- t.test(hardness ~ location, data = APPLE, var.equal=TRUE)$conf
CI

########################### R Code 8.13 ##########################

nx <- 15
ny <- 11
xbar <- 63/nx
ybar <- 66.4/ny
pe <- xbar - ybar
s2x <- (338 - nx*xbar^2)/(nx - 1)
s2y <- (486 - ny*ybar^2)/(ny - 1)
se <- sqrt(s2x/nx + s2y/ny)
nu <- (s2x/nx + s2y/ny)^2 / 
  ((s2x/nx)^2/(nx - 1) + (s2y/ny)^2/(ny - 1))
ct <- qt(0.975, nu)
c(xbar, ybar, pe, s2x, s2y, se, nu, ct)
CI <- pe + c(-1, 1)*ct*se
CI
tsum.test(mean.x = xbar, mean.y = ybar, s.x = sqrt(s2x), s.y = sqrt(s2y), 
          n.x = nx, n.y = ny, conf.level =0.95)$conf

########################### R Code 8.14 ##########################

t.test(CALCULUS$score[CALCULUS$calculus =="Yes"], 
       CALCULUS$score[CALCULUS$calculus =="No"])$conf
levels(CALCULUS$calculus)
CALCULUS$calculus <- factor(CALCULUS$calculus, levels = c("Yes", "No"))
t.test(score ~ calculus, data = CALCULUS)$conf


########################### Figure 8.7 ##########################

with(data = SUNDIG, qqnorm(difference))
with(data = SUNDIG, qqline(difference))


########################### Example 8.17 ##########################

qt(0.025, 4)
t.test(SUNDIG$sun, SUNDIG$digital, paired=TRUE)$conf


########################### Figure 8.8 ##########################


with(data = SUNDIG, ntester(difference))


########################### Figure 8.9 ##########################


LV <- 22
DF <- 6
curve(dchisq(x, DF), 0, LV, axes = FALSE, ann = FALSE, n = 500)
alpha <- 0.05
x <- seq(0, qchisq(alpha/2, 6), length = 100)
y <- dchisq(x, DF)
xs <- c(0, x, qchisq(alpha/2, DF))
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
x <- seq(qchisq(1 - alpha/2, DF), LV, length = 100)
y <- dchisq(x, DF)
xs <- c(qchisq(1 - alpha/2, DF), x, LV)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(0, 0, LV, 0, lwd = 3)
curve(dchisq(x, DF), 0, LV, axes = FALSE, ann = FALSE, add = TRUE, lwd = 2)
segments(qchisq(alpha/2, DF), 0, qchisq(alpha/2, DF), dchisq(qchisq(alpha/2, DF), DF), lwd = 2)
segments(qchisq(1 - alpha/2, DF), 0, qchisq(1 - alpha/2, DF), dchisq(qchisq(1 - alpha/2, DF), DF), lwd = 2)
text(5.5, 0.04, "$1 - \\alpha$")
mtext("$\\chi^2_{\\alpha/2;6}$", side = 1, line = 0, at = qchisq(alpha/2, DF))
mtext("$\\chi^2_{1-\\alpha/2;6}$", side = 1, line = 0, at = qchisq(1-alpha/2, DF))


########################### Example 8.18 ##########################

lchi <- qchisq(0.1, 14)
uchi <- qchisq(0.9, 14)
lep <- (15 - 1)*5.2449/uchi
uep <- (15 - 1)*5.2449/lchi
c(lep, uep)


########################### Figure 8.10 ##########################

with(data = barley, qqnorm(yield[year == "1932"]))
with(data = barley, qqline(yield[year == "1932"]))

########################### R Code 8.15 ##########################

library(lattice) 
BarleyYield1932 <- subset(barley, select = yield, 
                          subset = year == "1932", drop = TRUE)
n <- sum(!is.na(BarleyYield1932))
n
mean(BarleyYield1932)
var(BarleyYield1932)
sd(BarleyYield1932)
qt(.975, n-1)

t.test(BarleyYield1932, conf.level = 0.95)$conf
 
s2 <- var(BarleyYield1932)
lchi <- qchisq(0.025, n - 1)
uchi <- qchisq(0.975, n - 1)
c(s2, lchi, uchi)
ll <- (n - 1)*s2/uchi
ul <- (n - 1)*s2/lchi
CI <- c(ll, ul)
CI


########################### Figure 8.11 ##########################

LV <- 6
DF1 <- 10
DF2 <- 10
curve(df(x, DF1, DF2), 0, LV, axes = FALSE, ann = FALSE, n = 500)
alpha <- 0.05
x <- seq(0, qf(alpha/2, DF1, DF2), length = 100)
y <- df(x, DF1, DF2)
xs <- c(0, x, qf(alpha/2, DF1, DF2))
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
x <- seq(qf(1 - alpha/2, DF1, DF2), LV, length = 100)
y <- df(x, DF1, DF2)
xs <- c(qf(1 - alpha/2, DF1, DF2), x, LV)
ys <- c(0, y, 0)
polygon(xs, ys, col = "skyblue3")
segments(0, 0, LV, 0, lwd = 3)
curve(df(x, DF1, DF2), 0, LV, axes = FALSE, ann = FALSE, add = TRUE, lwd = 2)
segments(qf(alpha/2, DF1, DF2), 0, qf(alpha/2, DF1, DF2), df(qf(alpha/2, DF1, DF2), DF1, DF2), lwd = 2)
segments(qf(1 - alpha/2, DF1, DF2), 0, qf(1 - alpha/2, DF1, DF2), df(qf(1 - alpha/2, DF1, DF2), DF1, DF2), lwd = 2)
text(1.5, 0.1, "$1 - \\alpha$")
mtext("$f_{\\alpha/2;10, 10}$", side = 1, line = 0, at = qf(alpha/2, DF1, DF2))
mtext("$f_{1-\\alpha/2;10, 10}$", side = 1, line = 0, at = qf(1-alpha/2, DF1, DF2))

########################### Example 8.20 ##########################

lf <- qf(0.05, 10, 14)  # lower f value
uf <- qf(0.95, 10, 14)  # upper f value
c(lf, uf)
lep <- lf*2.481/2.1  # lower CI point
uep <- uf*2.481/2.1  # upper CI point
CI <- c(lep, uep)
CI

########################### R Code 8.16 ##########################

VAR <- tapply(APPLE$hardness, APPLE$location, var)
VAR
RVAR <- VAR[2]/VAR[1]
names(RVAR) <- NULL
lf <- qf(0.025, 16, 16)  # lower f value
uf <- qf(0.975, 16, 16)  # upper f value
c(lf, uf, RVAR)
lep <- lf*RVAR  # lower CI end point
uep <- uf*RVAR  # upper CI end point
CI <- c(lep, uep)
CI
# using var.test()
levels(APPLE$location)  # show default levels of location
APPLE$location <- factor(APPLE$location, 
                         levels = c("warehouse", "fresh"))
levels(APPLE$location)  # changed levels of location
var.test(hardness ~ location, data = APPLE)$conf

########################### R code 8.17 ##########################

n <- 25
p <- 15/n                   # sample proportion
alpha <- 0.10               # alpha level 
z <- qnorm(1 - alpha/2)     # critical value
me <- z*sqrt(p*(1 - p)/n)   # margin of error
CI <- p + c(-1, 1)*me
CI
library(binom)
binom.confint(x = 15, n = 25, conf.level = 0.90, methods = "asymptotic")

########################### R Code 8.18 ##########################

n <- 25
alpha <- 0.05
x <- 0:n
p <- x/n
m.err <- qnorm(1 - alpha/2)*sqrt(p*(1 - p)/n)
lcl <- p - m.err           # lower confidence limit
ucl <- p + m.err           # upper confidence limit
PI <- 0.70                 # PI = P(Success)
prob <- dbinom(x, n, PI)   # binomial probability
cover <- (PI >= lcl) & (PI <= ucl)  # vector of 0s and 1s
RES <- cbind(x, p , lcl, ucl, prob, cover)
RES[12:24, ]               # show only rows 12-24  
# P(Cover) = P(X = 13) + P(X = 14) + P(X = 15) + ... + P(X = 21)
CP <- sum(dbinom(x[cover], n, PI))  # coverage probability
CP
 
library(binom)
binom.coverage(p = 0.70, n = 25, conf = 0.95, method = "asymptotic")

########################### R Code 8.19 ##########################

n <- 25
alpha <- 0.05
x <- 0:n
p <- x/n
m.err <- qnorm(1 - alpha/2)*sqrt(p*(1 - p)/n)
lcl <- p - m.err           # lower confidence limit
ucl <- p + m.err           # upper confidence limit
PI <- 0.69                 # PI = P(Success)
prob <- dbinom(x, n, PI)   # binomial probability
cover <- (PI >= lcl) & (PI <= ucl)  # vector of 0s and 1s
RES <- cbind(x, p , lcl, ucl, prob, cover)
RES[12:23, ]               # show only rows 12-23  
# P(Cover) = P(X = 13) + P(X = 14) + P(X = 15) + ... + P(X = 20)
CPb <- sum(dbinom(x[cover], n, PI))  # coverage probability
CPb
 
library(binom)
binom.coverage(p = 0.69, n = 25, conf = 0.95, method = "asymptotic")

########################### R Code 8.20 ##########################

library(binom)

n <- 25
alpha <- 0.05
CL <- 1 - alpha
z <- qnorm(1 - alpha/2)
x <- 0:n
p <- x/n
m.err <- z*sqrt(p*(1 - p)/n)
lcl <- p - m.err
ucl <- p + m.err
m <- 2000
PI <- seq(1/m, 1 - 1/m, length = m)
p.cov <- numeric(m)
for (i in 1:m)
{
 cover <- (PI[i] >= lcl) & (PI[i] <= ucl)
 p.rel <- dbinom(x[cover], n, PI[i])
 p.cov[i] <- sum(p.rel)
}
plot(PI, p.cov, type = "l", ylim = c(0.0, 1.05), main = "n = 25",
    xlab = expression(pi), ylab = "Coverage Probability")
lines(c(1/m, 1 - 1/m), c(1 - alpha, 1 - alpha), col = "red",
     lty = "dashed")
text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
#
binom.plot(n = 25, method = binom.asymp, np = 2000)

############################### Binom.plot ###########################

Binom.plot <- function (n, method = binom.lrt, np = 500, conf.level = 0.95, 
                        actual = conf.level, type = c("xyplot", "levelplot"), tol = .Machine$double.eps^0.5, 
                        ...) 
{
  stopifnot(require(lattice))
  type <- match.arg(type)
  if (length(n) != 1) {
    if (length(n) > 1 && type == "levelplot") {
      warning(sprintf("n must be of length 1, not %d", 
                      length(n)))
      n <- n[1]
    }
  }
  E.pn <- function(x, n, p, lower, upper) (p >= lower & p <= 
                                             upper) * dbinom(x, n, p)
  p <- seq(tol, 1 - tol, length = np)
  args <- list(...)
  if (type == "levelplot") {
    x <- 0:n
    ci <- method(x, n, conf.level, ...)[c("x", "n", "lower", 
                                          "upper")]
    z <- merge(ci, data.frame(p = p))
    z$coverage <- with(z, E.pn(x, n, p, lower, upper))
    z$n <- factor(sprintf("n = %d", n))
    args$x <- coverage ~ p * x | n
    if (is.null(args$col.regions)) 
      args$col.regions <- heat.colors(100)[100:1]
    if (is.null(args$panel)) 
      args$panel <- panel.binom.plot.levelplot
    args$breaks <- ci
    if (is.null(args$scales)) 
      args$scales <- list(y = list(at = x, labels = x))
  }
  else {
    x <- unlist(lapply(lapply(n, ":", 0), rev))
    n <- rep(n, n + 1)
    ci <- method(x, n, conf.level, ...)[c("x", "n", "lower", 
                                          "upper")]
    ci$lower <- ifelse(ci$lower < 0, 0, ci$lower)
    ci$upper <- ifelse(ci$upper > 1, 1, ci$upper)
    z <- merge(ci, data.frame(p = p))
    z$coverage <- with(z, E.pn(x, n, p, lower, upper))
    z <- aggregate(z["coverage"], z[c("p", "n")], sum)
    args$x <- coverage ~ p | n
    args$n <- z$n
    z$n <- factor(z$n, labels = sprintf("n = %d", sort(unique(z$n))))
    args$breaks <- ci[c("n", "lower", "upper")]
    if (is.null(args$panel)) 
      args$panel <- panel.binom.plot.xyplot
    if (is.null(args$ylab)) {
      args$ylab <- "E(p$|$n)"         # expression(E(paste(p, "|", n)))
    }
    args$conf.level <- conf.level
    args$actual <- actual
  }
  args$data <- z
  if (is.null(args$as.table)) 
    args$as.table <- TRUE
  do.call(type, args)
}

binom.lrt <- function (x, n, conf.level = 0.95, bayes = FALSE, conf.adj = FALSE, 
                       plot = FALSE, ...) 
{
  do.plot <- ((is.logical(plot) && plot) || is.list(plot)) && 
    require(lattice)
  xn <- cbind(x = x, n = n)
  ok <- !is.na(xn[, 1]) & !is.na(xn[, 2])
  x <- xn[ok, "x"]
  n <- xn[ok, "n"]
  p <- ifelse(ok, x/n, NA)
  res <- data.frame(xn, mean = p)
  alpha <- 1 - conf.level
  alpha <- rep(alpha, length = length(p))
  res$lower <- rep(0, NROW(res))
  res$upper <- rep(1, NROW(res))
  args <- list(...)
  tol <- if (is.null(args$tol)) 
    .Machine$double.eps^0.5
  else args$tol
  bindev <- function(y, x, mu, wt, bound = 0, tol = .Machine$double.eps^0.5, 
                     ...) {
    ll.y <- ifelse(y %in% c(0, 1), 0, ldbinom(x, wt, y))
    ll.mu <- ifelse(mu %in% c(0, 1), 0, ldbinom(x, wt, mu))
    f <- ifelse(abs(y - mu) < tol, 0, sign(y - mu) * sqrt(-2 * 
                                                            (ll.y - ll.mu)))
    f - bound
  }
  args$f <- bindev
  x0 <- x == 0
  xn <- x == n
  z <- qnorm(1 - alpha * ifelse((x0 | xn) & conf.adj, 0.25, 
                                0.5))
  if ((is.logical(bayes) && bayes) || is.numeric(bayes)) {
    if (is.logical(bayes)) {
      bayes <- c(0.5, 0.5)
    }
    else if (length(bayes) == 1) {
      bayes <- c(bayes, bayes)
    }
    if (any(edge <- x0 | xn)) {
      n[edge] <- n[edge] + bayes[1] + bayes[2]
      x[edge] <- x[edge] + bayes[1]
    }
    p <- x/n
    bayes <- TRUE
  }
  plot.df <- list()
  for (i in seq(NROW(res))) {
    if (!ok[i]) {
      res$lower[i] <- res$upper[i] <- NA
      next
    }
    args[c("x", "mu", "wt")] <- list(x = x[i], mu = p[i], 
                                     wt = n[i])
    if (!x0[i] && tol < p[i]) {
      args$interval <- c(tol, if (p[i] < tol || p[i] == 
                                    1) 1 - tol else p[i])
      args$bound <- -z[i]
      res$lower[i] <- if (bindev(tol, x[i], p[i], n[i], 
                                 -z[i], tol) > 0) {
        if (conf.adj) 
          z[i] <- qnorm(1 - alpha)
        0
      }
      else {
        do.call("uniroot", args)$root
      }
    }
    if (!xn[i] && p[i] < 1 - tol) {
      args$interval <- c(if (p[i] > 1 - tol) tol else p[i], 
                         1 - tol)
      args$bound <- z[i]
      res$upper[i] <- if (bindev(1 - tol, x[i], args$interval[1], 
                                 n[i], z[i], tol) < 0) {
        args$interval <- c(tol, p[i])
        if (conf.adj) 
          z[i] <- qnorm(1 - alpha)
        args$bound <- -z[i]
        res$lower[i] <- if (bindev(tol, x[i], if (p[i] < 
                                                    tol || p[i] == 1) 
          1 - tol
          else p[i], n[i], -z[i], tol) > 0) {
          0
        }
        else {
          do.call("uniroot", args)$root
        }
        1
      }
      else {
        do.call("uniroot", args)$root
      }
    }
    if (do.plot) {
      min.p <- res$lower[i]
      max.p <- res$upper[i]
      dx.p <- 0.1 * (max.p - min.p)
      expand.p <- seq(max(min.p - dx.p, tol), min(max.p + 
                                                    dx.p, 1 - tol), len = 100)
      plot.df[[i]] <- data.frame(p = expand.p)
      plot.df[[i]]$x <- res$x[i]
      plot.df[[i]]$n <- res$n[i]
      plot.df[[i]]$lower <- res$lower[i]
      plot.df[[i]]$upper <- res$upper[i]
      plot.df[[i]]$mean <- res$mean[i]
      plot.df[[i]]$deviance <- bindev(expand.p, x[i], p[i], 
                                      n[i], 0, tol)
    }
  }
  if (do.plot) {
    plot.df <- do.call("rbind", plot.df)
    xn <- unique(data.frame(x = plot.df$x, n = plot.df$n))
    xn <- xn[order(xn$x, xn$n), ]
    lev.xn <- with(xn, sprintf("x = %s; n = %s", x, n))
    plot.df$xn <- with(plot.df, factor(sprintf("x = %s; n = %s", 
                                               x, n), levels = lev.xn))
    plot.args <- if (is.list(plot)) 
      plot
    else list()
    plot.args$x <- deviance ~ p | xn
    plot.args$data <- plot.df
    plot.args$lower <- plot.df$lower
    plot.args$upper <- plot.df$upper
    plot.args$mean <- plot.df$mean
    plot.args$alpha <- unique(alpha)
    if (is.null(plot.args$panel)) 
      plot.args$panel <- panel.binom.lrt
    if (is.null(plot.args$xlab)) 
      plot.args$xlab <- "Probability of Success (p)"
    if (is.null(plot.args$ylab)) 
      plot.args$ylab <- "Standard Normal Quantiles"
    if (is.null(plot.args$par.strip.text)) {
      plot.args$par.strip.text <- list(cex = 1.2)
    }
    else if (is.null(plot.args$par.strip.text)) {
      plot.args$par.strip.text$cex <- 1.2
    }
    if (is.null(plot.args$as.table)) 
      plot.args$as.table <- TRUE
    print(do.call("xyplot", plot.args))
  }
  attr(res, "conf.level") <- conf.level
  cbind(method = "lrt", res)
}

### Internal Stuff
var.asymp <- function(p, n = 1) {
  p * (1 - p)/n
}

var.cloglog <- function(p, n = 1) {
  mu <- log(p)
  (1 - p)/n/p/mu^2
}

var.logit <- function(p, n = 1) {
  1/n/p/(1 - p)
}

var.probit <- function(p, n = 1) {
  z <- qnorm(p)
  p * (1 - p)/n/dnorm(z)^2
}

binom.lchoose <- function(n, x) {
  lgamma(n + 1) - lgamma(n - x + 1) - lgamma(x + 1)
}

ldbinom <- function(x, size, prob, log = TRUE) {
  log.f <- binom.lchoose(size, x) + x * log(prob) + (size - x) * log(1 - prob)
  if(log) log.f else exp(log.f)
}
####
panel.binom.plot.levelplot <- function(x, y, z, subscripts, breaks = NULL, ...) {
  panel.levelplot(x, y, z, subscripts, ...)
  if(!is.null(breaks)) {
    breaks <- breaks[subscripts, ]
    for(i in seq(nrow(breaks))) {
      x.i <- with(breaks, c(lower[i], upper[i]))
      y.i <- breaks$x[i]
      p.x <- x.i[c(1, 2, 2, 1)]
      p.y <- y.i + c(-0.5, -0.5, 0.5, 0.5)
      lpolygon(p.x, p.y, border = "#cccccc", lwd = 3)
    }
  }
}

panel.binom.plot.xyplot <-  function(x, y, subscripts, conf.level, n, breaks, actual, ...) {
  panel.abline(h = actual, lty = 2, lwd = 2, col = "#880000")
  n <- unique(n[subscripts])
  breaks <- unique(sort(unlist(breaks[breaks$n == n, 2:3])))
  nb <- length(breaks)
  if(any(m <- breaks %in% x))
    breaks[m] <- ifelse(breaks[m] > 0.5, breaks[m] - 1e-8, breaks[m] + 1e-8)
  x <- c(x, breaks)
  y <- c(y, rep(NA, nb))
  x <- x[ord <- order(x)]
  y <- y[ord]
  panel.xyplot(x, y, type = "l", ...)
  xx <- rep(breaks, each = 3)
  xx[seq(3, nb * 3, 3)] <- NA
  na <- which(is.na(y))
  wh.y <- rep(na, each = 3) + rep(c(-1, 1, 0), times = length(na))
  ny <- length(y)
  zero <- wh.y == 0
  ny.plus.1 <- wh.y == ny + 1
  end <- zero | ny.plus.1
  yy <- y[wh.y[!end]]
  if(any(end)) {
    if(any(zero)) yy <- c(NA, yy)
    if(any(ny.plus.1)) yy <- c(yy, NA)
  }
  yy[wh.y %in% na] <- NA
  panel.xyplot(xx, yy, type = "l", lty = 4, lwd = 2, col = "#888888")
}
###
binom.methods <- c("wilson", "agresti-coull", "ac", "exact", "prop.test", "profile", "lrt",
                   "asymptotic", "bayes", "cloglog", "logit", "probit", "all")

binom.confint <- function(x, n, conf.level = 0.95, methods = "all", ...) {
  if((length(x) != length(n))) {
    m <- cbind(x = x, n = n)
    x <- m[, "x"]
    n <- m[, "n"]
  }
  res <- NULL
  method <- pmatch(methods, binom.methods)
  if(all(is.na(method))) {
    methods <- paste(paste("\"", methods, "\"", sep = ""), collapse = ", ")
    stop("method(s) ", methods, "\" not matched")
  }
  if(any(is.na(method))) {
    warning("method(s) ", methods[is.na(method)], " not matched")
    method <- methods[!is.na(method)]
  }
  method <- binom.methods[method]
  out <- x > n | x < 0
  if(any(out)) {
    warning("observations with more successes than trials detected and removed")
    x <- x[!out]
    n <- n[!out]
  }
  xn <- data.frame(x = x, n = n)
  all.methods <- any(method == "all")
  p <- x/n
  alpha <- 1 - conf.level
  alpha <- rep(alpha, length = length(p))
  alpha2 <- 0.5 * alpha
  z <- qnorm(1 - alpha2)
  z2 <- z * z
  res <- NULL
  if(any(method %in% c("agresti-coull", "ac")) || all.methods) {
    .x <- x + 0.5 * z2
    .n <- n + z2
    .p <- .x/.n
    lcl <- .p - z * sqrt(.p * (1 - .p)/.n)
    ucl <- .p + z * sqrt(.p * (1 - .p)/.n)
    res.ac <- data.frame(method = rep("agresti-coull", NROW(x)),
                         xn, mean = p, lower = lcl, upper = ucl)
    res <- res.ac
  }
  if(any(method == "asymptotic") || all.methods) {
    se <- sqrt(p * (1 - p)/n)
    lcl <- p - z * se
    ucl <- p + z * se
    res.asymp <- data.frame(method = rep("asymptotic", NROW(x)),
                            xn, mean = p, lower = lcl, upper = ucl)
    res <- if(is.null(res)) res.asymp else rbind(res, res.asymp)
  }
  if(any(method == "bayes") || all.methods) {
    res.bayes <- binom.bayes(x, n, conf.level = conf.level, ...)
    res.bayes <- res.bayes[c("method", "x", "n", "mean", "lower", "upper")]
    res <- if(is.null(res.bayes)) res.bayes else rbind(res, res.bayes)
  }
  if(any(method == "cloglog") || all.methods) {
    if(any(method != "exact")) {
      x1 <- x == 0
      x2 <- x == n
    }
    inner <- !x1 & !x2
    log.mu <- sd <- lcl <- ucl <- rep(NA, length(x))
    log.mu[inner] <- log(-log(p[inner]))
    sd[inner] <- sqrt(var.cloglog(p[inner], n[inner]))
    lcl[inner] <- exp(-exp(log.mu[inner] + z[inner] * sd[inner]))
    ucl[inner] <- exp(-exp(log.mu[inner] - z[inner] * sd[inner]))
    lcl[x1] <- rep(0, sum(x1))
    lcl[x2] <- alpha2[x2]^(1/n[x2])
    ucl[x1] <- 1 - alpha2[x1]^(1/n[x1])
    ucl[x2] <- rep(1, sum(x2))
    res.cloglog <- data.frame(method = rep("cloglog", NROW(x)),
                              xn, mean = p, lower = lcl, upper = ucl)
    res <- if(is.null(res)) res.cloglog else rbind(res, res.cloglog)
  }
  if(any(method == "exact") || all.methods) {
    x1 <- x == 0
    x2 <- x == n
    lb <- ub <- x
    lb[x1] <- 1
    ub[x2] <- n[x2] - 1
    lcl <- 1 - qbeta(1 - alpha2, n + 1 - x, lb)
    ucl <- 1 - qbeta(alpha2, n - ub, x + 1)
    if(any(x1)) lcl[x1] <- rep(0, sum(x1))
    if(any(x2)) ucl[x2] <- rep(1, sum(x2))
    res.exact <- data.frame(method = rep("exact", NROW(x)),
                            xn, mean = p, lower = lcl, upper = ucl)
    res <- if(is.null(res)) res.exact else rbind(res, res.exact)
  }
  if(any(method == "logit") || all.methods) {
    if(any(method != "exact")) {
      x1 <- x == 0
      x2 <- x == n
    }
    inner <- !x1 & !x2
    logit.p <- sd <- lcl <- ucl <- rep(NA, length(x))
    logit.p[inner] <- log(p[inner]) - log1p(-p[inner])
    sd[inner] <- sqrt(var.logit(p[inner], n[inner]))
    .lcl <- exp(logit.p[inner] - z[inner] * sd[inner])
    .ucl <- exp(logit.p[inner] + z[inner] * sd[inner])
    lcl[inner] <- .lcl/(1 + .lcl)
    ucl[inner] <- .ucl/(1 + .ucl)
    lcl[x1] <- rep(0, sum(x1))
    lcl[x2] <- alpha2[x2]^(1/n[x2])
    ucl[x1] <- 1 - alpha2[x1]^(1/n[x1])
    ucl[x2] <- rep(1, sum(x2))
    res.logit <- data.frame(method = rep("logit", NROW(x)),
                            xn, mean = p, lower = lcl, upper = ucl)
    res <- if(is.null(res)) res.logit else rbind(res, res.logit)
  }
  if(any(method == "probit") || all.methods) {
    if(any(method != "exact")) {
      x1 <- x == 0
      x2 <- x == n
    }
    inner <- !x1 & !x2
    probit.p <- sd <- lcl <- ucl <- rep(NA, length(x))
    probit.p[inner] <- qnorm(p[inner])
    sd[inner] <- sqrt(var.probit(p[inner], n[inner]))
    lcl[inner] <- pnorm(probit.p[inner] - z[inner] * sd[inner])
    ucl[inner] <- pnorm(probit.p[inner] + z[inner] * sd[inner])
    lcl[x1] <- rep(0, sum(x1))
    lcl[x2] <- alpha2[x2]^(1/n[x2])
    ucl[x1] <- 1 - alpha2[x1]^(1/n[x1])
    ucl[x2] <- rep(1, sum(x2))
    res.probit <- data.frame(method = rep("probit", NROW(x)),
                             xn, mean = p, lower = lcl, upper = ucl)
    res <- if(is.null(res)) res.probit else rbind(res, res.probit)
  }
  if(any(method == "profile") || all.methods) {
    res.prof <- binom.profile(x, n, conf.level, ...)
    res <- if(is.null(res)) res.prof else rbind(res, res.prof)
  }
  if(any(method == "lrt") || all.methods) {
    res.lrt <- binom.lrt(x, n, conf.level = conf.level, ...)
    res <- if(is.null(res)) res.lrt else rbind(res, res.lrt)
  }
  if(any(method == "prop.test") || all.methods) {
    ci <- lapply(seq_along(x), function(i) stats::prop.test(x[i], n[i])$conf.int)
    lcl <- sapply(ci, "[", 1)
    ucl <- sapply(ci, "[", 2)
    res.prop.test <- data.frame(method = rep("prop.test", NROW(x)),
                                xn, mean = p, lower = lcl, upper = ucl)
    res <- if(is.null(res)) res.prop.test else rbind(res, res.prop.test)
  }
  if(any(method == "wilson") || all.methods) {
    p1 <- p + 0.5 * z2/n
    p2 <- z * sqrt((p * (1 - p) + 0.25 * z2/n)/n)
    p3 <- 1 + z2/n
    lcl <- (p1 - p2)/p3
    ucl <- (p1 + p2)/p3
    # x1 <- x == 1
    # x2 <- x == n - 1
    # if(any(x1)) lcl[x1] <- -log(1 - alpha[x1])/n[x1]
    # if(any(x2)) ucl[x2] <- 1 + log(1 - alpha[x2])/n[x2]
    res.wilson <- cbind(method = rep("wilson", NROW(x)),
                        xn, mean = p, lower = lcl, upper = ucl)
    res <- if(is.null(res)) res.wilson else rbind(res, res.wilson)
  }
  attr(res, "conf.level") <- conf.level
  row.names(res) <- seq(nrow(res))
  res
}

binom.wilson <- function(x, n, conf.level = 0.95, ...)
  binom.confint(x, n, conf.level, "wilson")

binom.agresti.coull <- function(x, n, conf.level = 0.95, ...)
  binom.confint(x, n, conf.level, "ac")

binom.exact <- function(x, n, conf.level = 0.95, ...)
  binom.confint(x, n, conf.level, "exact")

binom.asymp <- function(x, n, conf.level = 0.95, ...)
  binom.confint(x, n, conf.level, "asymp")

binom.prop.test <- function(x, n, conf.level = 0.95, ...)
  binom.confint(x, n, conf.level, "prop.test")

binom.cloglog <- function(x, n, conf.level = 0.95, ...)
  binom.confint(x, n, conf.level, "cloglog")

binom.logit <- function(x, n, conf.level = 0.95, ...)
  binom.confint(x, n, conf.level, "logit")

binom.probit <- function(x, n, conf.level = 0.95, ...)
  binom.confint(x, n, conf.level, "probit")


################################### Figure 8.12 #####################################

n <- 25  # comment more
alpha <- 0.05 # comment two
CL <- 1 - alpha
z <- qnorm(1 - alpha/2)
x <- 0:n
p <- x/n
m.err <- z*sqrt(p*(1 - p)/n)
lcl <- p - m.err
ucl <- p + m.err
m <- 2000
PI <- seq(1/m, 1 - 1/m, length = m)
p.cov <- numeric(m)
for (i in 1:m)
{
  cover <- (PI[i] >= lcl) & (PI[i] <= ucl)
  p.rel <- dbinom(x[cover], n, PI[i])
  p.cov[i] <- sum(p.rel)
}
plot(PI, p.cov, type = "l", ylim = c(0.0, 1.05), main = "n = 25",
     xlab = "$\\pi$", ylab = "Coverage Probability")
lines(c(1/m, 1 - 1/m), c(1 - alpha, 1 - alpha), col = "red", 
      lty = "dashed")
text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
#
Binom.plot(n = 25, method = binom.asymp, np = 2000)
 
############################## Figure 8.13 ###############################

## Traditional and Agresti-Coull
n <- 20 # comment
alpha <- 0.05
x <- 0:n
p <- x/n
z <- qnorm(1 - alpha/2)
ntilde <- n + z^2
pAC <- (1/ntilde)*(x + 1/2*z^2)
m <- 10000
PI <- seq(1/m, 1 - 1/m, length = m)
m.err <- z*sqrt(p*(1 - p)/n)
m.errAC <- z*sqrt(pAC*(1 - pAC)/ntilde)
lcl <- p - m.err
lclAC <- pAC - m.errAC
ucl <- p + m.err
uclAC <- pAC + m.errAC
lengthTRAD <- ucl - lcl
lengthAC <- uclAC - lclAC
pcoverTRAD <- numeric(m)
pcoverAC <- numeric(m)
ELtrad <- numeric(m)
ELac <- numeric(m)
for(i in 1:m){
  cover <- (PI[i] >= lcl) & (PI[i] <= ucl)  # vector of 0s and 1s 
  coverAC <- (PI[i] >= lclAC) & (PI[i] <= uclAC)
  prelTrad <- dbinom(x[cover], n, PI[i])
  prelAC <- dbinom(x[coverAC], n, PI[i])
  pcoverTRAD[i] <- sum(prelTrad)
  pcoverAC[i] <- sum(prelAC)
  ELtrad[i] <- sum(lengthTRAD * dbinom(x, n, PI[i]))
  ELac[i] <- sum(lengthAC * dbinom(x, n, PI[i]))
}
# plot(PI, ELtrad, type = "l")
# lines(PI, ELac)
# Exact now
LCI <- numeric(n+1)
UCI <- numeric(n+1)
for(j in 1:(n+1)){
  LCI[j] <- binom.test(x[j], n, conf.level = 1 - alpha)$conf[1]
  UCI[j] <- binom.test(x[j], n, conf.level = 1 - alpha)$conf[2]
}
lengthEXACT <- UCI - LCI
pcoverEXACT <- numeric(m)
ELexact <- numeric(m)
for(i in 1:m){
  cover <- (PI[i] >= LCI) & (PI[i] <= UCI)  # vector of 0s and 1s 
  prelEXACT <- dbinom(x[cover], n, PI[i])
  pcoverEXACT[i] <- sum(prelEXACT)
  ELexact[i] <- sum(lengthEXACT * dbinom(x, n, PI[i]))
}
# lines(PI, ELexact, type = "l")
# Wilson Score
LCIw <- numeric(n+1)
UCIw <- numeric(n+1)
for(j in 1:(n+1)){
  LCIw[j] <- prop.test(x[j], n, conf.level = 1 - alpha, correct = FALSE)$conf[1]
  UCIw[j] <- prop.test(x[j], n, conf.level = 1 - alpha, correct = FALSE)$conf[2]
}
lengthWilson <- UCIw - LCIw
pcoverWilson <- numeric(m)
ELwilson <- numeric(m)
for(i in 1:m){
  coverWilson <- (PI[i] >= LCIw) & (PI[i] <= UCIw)  # vector of 0s and 1s 
  prelWilson <- dbinom(x[coverWilson], n, PI[i])
  pcoverWilson[i] <- sum(prelWilson)
  ELwilson[i] <- sum(lengthWilson * dbinom(x, n, PI[i]))
}
# lines(PI, ELwilson, type = "l")
####  ggplot2 now
DF <- data.frame(Interval = rep(c("Wald", "Clopper-Pearson",
                                  "Agresti-Coull", "Wilson"),
                                  each = m), Pi = rep(PI, 4), 
                ProbCoverage = c(pcoverTRAD, pcoverEXACT, 
                                 pcoverAC, pcoverWilson),
                Elength = c(ELtrad, ELexact, ELac, ELwilson))


library(ggplot2) # load
ggplot(data = DF, aes(x = Pi, y = ProbCoverage)) + 
  geom_line() + facet_wrap( ~ Interval, nrow = 2) + 
  geom_hline(yintercept = 1 - alpha, linetype = "dashed") + 
  labs(x = "$\\pi$", y = "Probability Coverage") + 
  theme_bw() + 
  ylim(0.7, 1.0)

########################### Figure 8.14 ########################

ggplot(data = DF, aes(x = Pi, y = Elength, color = Interval,
                      linetype = Interval )) + 
  geom_line(size = 0.7) +
  labs(x = "$\\pi$", y = "E(Width)") + theme_bw()

######################### R Code 8.21 ########################

n <- 40
x <- 26
p <- x/n            # sample proportion passing
z <- qnorm(0.975)   # z_{0.975} 
n <- 40
CI <- p + c(-1, 1)*z*sqrt(p*(1 - p)/n)
CI 
# Or
library(binom)
binom.confint(x = 26, n = 40, conf.level = 0.95, methods = "asymptotic")

######################### R Code 8.22 ########################

prop.test(x = 26, n = 40, correct = FALSE, conf.level = 0.95)$conf
# Or
binom.confint(x = 26, n = 40, conf.level = 0.95, methods = "wilson")
 
prop.test(x = 26, n = 40, correct = TRUE, conf.level = 0.95)$conf
 

######################### R Code 8.23 ########################
 
 
z <- qnorm(0.975)
ntilde <- 40 + z^2
ntilde
ptilde <- (1/ntilde)*(26 + 1/2*z^2)
ptilde
me <- z*sqrt(ptilde*(1 - ptilde)/ntilde)
CI <- ptilde + c(-1, 1)*me
CI
# Or
binom.confint(x = 26, n = 40, methods = "ac")

######################### R Code 8.24 ########################

CI <- c(qbeta(0.025, 26, 40 - 26 + 1), qbeta(0.975, 26 + 1, 40 - 26))
CI
# Or
binom.confint(x = 26, n = 40, conf = 0.95, method = "exact")

######################### R Code 8.25 ########################

CI1 <- prop.test(x = 157, n = 200, conf = 0.90, correct = FALSE)$conf
CI1
# Or
binom.confint(x = 157, n = 200, conf = 0.90, methods = "wilson")
CI2 <- prop.test(x = 157, n = 200, conf = 0.95, correct = FALSE)$conf
CI2
# Or
binom.confint(x = 157, n = 200, conf = 0.95, methods = "wilson")
CI3 <- prop.test(x = 157, n = 200, conf = 0.99, correct = FALSE)$conf
CI3
# Or
binom.confint(x = 157, n = 200, conf = 0.99, methods = "wilson")

######################### Example 8.27 ########################

x <- 400
n <- 2000
p <- x/n
z <- qnorm(0.975)  # z_{0.975}
CI <- p + c(-1, 1)*z*sqrt(p*(1 - p)/n)
CI
# Or
binom.confint(x = 400, n = 2000, conf = 0.95, methods = "asymptotic")
 
f <- function(x){sqrt(x*(1 - x))}  # x takes place of p in f
curve(expr = f, from = 0, to = 1, n = 500)

######################### R Code 8.26 ########################

nsize(b = 0.015, type = "pi", conf.level = 0.95)

######################### Example 8.28 ########################

prop.test(x = c(70, 90), n = c(1400, 2000), conf.level = 0.95, 
          correct = FALSE)$conf
 

######################### R Code 8.27 ########################

n <- sum(!is.na(SOCCER$game))  # number of games
xbar <- mean(SOCCER$goals, na.rm = TRUE)
z <- qnorm(0.95)  # z_{0.95}
CI <- xbar + c(-1, 1)*z*sqrt(xbar/n)
CI

