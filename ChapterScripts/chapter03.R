################### Chapter 03 Script
################### 7/24/15 V.1 
library(PASWR2)
library(MASS)
#################### Example 3.3 ##################

factorial(4)/factorial(4-3)  

#################### Example 3.4 ##################

factorial(7)/factorial(7 - 7)

#################### Example 3.5 ##################

factorial(4)/(factorial(2)*factorial(1)*factorial(1))


#################### Example 3.6 ##################

choose(n = 8, k = 3)

#################### Example 3.8 ##################

#################### R Code 3.1  ##################

choose(9, 2)*choose(7, 3)*choose(4, 4)
factorial(9)/(factorial(2)*factorial(3)*factorial(4))
choose(9,2)*(factorial(7)/(factorial(3)*factorial(4)))


#################### Example 3.9 ##################

#################### R Code 3.2  ##################

m <- seq(10, 50, 5)
P.E <- function(m){
  c(Students = m, ProbAtL2SB = 1 - prod((365:(365 - m + 1)/365)))
}
t(sapply(m, P.E))

#################### R Code 3.3 ##################
#################### Figure 3.1 ##################


m <- 1:60          # vector of number of students
p <- numeric(60)   # initialize vector to 0's
for(i in m){       # index values for loop
  q = prod((365:(365 - i + 1))/365) # P(No Match) if i people in room
  p[i] = 1 - q} 
plot(m, p, col = "skyblue3", pch = 19,
     ylab = "P(at least 2 students with the same birthday)",
     xlab = "m = Number of students in the room")
abline(h = 0.5, lty = 2, col = "red")      # Add horizontal line
abline(v = 23, lty = 2, col = "red")       # Add vertical line



#################### Example 3.11 ##################

#################### R Code  3.4  ##################
  
library(MASS)  # used for fractions function
Omega <- expand.grid(roll1 = 1:6, roll2 = 1:6)
H <- subset(Omega, roll1 + roll2 == 8)
H
G <- subset(Omega, roll1 == 4)
G
PG <- dim(G)[1]/dim(Omega)[1]  # P(G)
fractions(PG)
HaG <- subset(Omega, roll1 == 4 & roll2 ==4)  # event H and G
HaG
PHaG <- dim(HaG)[1]/dim(Omega)[1]  # P(H and G)
fractions(PHaG)
PHgG <- PHaG/PG  # P(H|G)
fractions(PHgG)

#################### R Code 3.5 ##################

library(MASS)
Omega <- expand.grid(roll1 = 1:6, roll2 = 1:6)
G <- subset(Omega, roll1 == 4)        # event G 
G
HgG <- subset(G, roll1 + roll2 == 8)  # event H|G
HgG
HgG <- subset(G, roll2 == 4)          # event H|G
HgG
HgG <- subset(G, roll2 %in% 4)        # event H|G
HgG
PHgG <- dim(HgG)[1]/dim(G)[1]         # P(H|G)
fractions(PHgG)


#################### Figure 3.2 ##################

plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
segments(0, 0, 1, 0)
segments(0, 1, 1, 1)
segments(0, 0, 0, 1)
segments(1, 0, 1, 1)
segments(.55,0,.55,1)
symbols( .53, .5, circles = 0.05, add = TRUE, inches = FALSE)
text(.1, .9, "$A$")
text(.9, .9, "$B$")
text(.5, .5, "$C$")
text(.25, .27, "$\\gil{P}(C \\cap A) = 0.0275$")
text(.75, .73, "$\\gil{P}(C \\cap B) = 0.018$")
arrows(.28, .31, .47, .45, length = .1)
arrows(.78, .7, .59, .55, length = .1)
text(.1, .1, "$\\gil{P}(A) = 0.55$")
text(.9, .1, "$\\gil{P}(B) = 0.45$")



#################### Example 3.15 ##################

#################### R Code 3.6   ##################

set.seed(2)  # done for reproducibility
actual <- sample(x = 1:3, size = 10000, replace = TRUE)
aguess <- sample(x = 1:3, size = 10000, replace = TRUE)
equals <- (actual == aguess)
PNoSwitch <- sum(equals)/10000
not.eq <- (actual != aguess)
PSwitch <- sum(not.eq)/10000
Probs <- c(PNoSwitch, PSwitch)
names(Probs) <- c("P(Win no Switch)", "P(Win Switch)")
Probs



#################### Example 3.17 ##################

#################### R Code 3.7   ##################

opar <- par(no.readonly = TRUE)
library(MASS)  # used for fractions function
par(mfrow=c(1, 2), pty = "s")
Omega <- expand.grid(coin1 = 0:1, coin2 = 0:1, coin3 = 0:1)
n.heads <- apply(Omega, 1, sum)
cbind(Omega, n.heads)

T1 <- table(n.heads)/length(n.heads)
fractions(T1)

plot(T1, xlab = "x", ylab="P(X = x)", yaxt = "n", main = "PDF for X")
axis(2, at = c(1/8, 3/8), labels = c("1/8", "3/8"), las = 1)
plot(ecdf(n.heads), main = "CDF for X", ylab = "F(x)", xlab = "x", 
     yaxt = "n")
axis(2, at = c(1/8, 4/8, 7/8, 1), labels = c("1/8", "4/8", "7/8", "1"), 
     las = 1)
segments(1, 1/8, 1, 4/8, lty = 2)
text(2.6, 2.5/8, "P(X = 1) =  F(1) - F(0)")
par(opar)



#################### Figure 3.4 ##################

opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), pty = "s")
Omega <- expand.grid(coin1 = 0:1, coin2 = 0:1, coin3 = 0:1)
n.heads <- apply(Omega, 1, sum)
plot(table(n.heads)/length(n.heads), xlab = "$x$", ylab="$\\gil{P}(X = x)$", yaxt="n", main = "PDF for $X$")
axis(2, at=c(1/8,3/8), labels=c("$\\frac{1}{8}$","$\\frac{3}{8}$"), las=1)
plot(ecdf(n.heads), main = "CDF for $X$", ylab = "$F(x)$", xlab = "$x$", yaxt="n")
axis(2, at=c(1/8, 4/8, 7/8, 1), labels=c("$\\frac{1}{8}$","$\\frac{4}{8}$","$\\frac{7}{8}$","$1$"), las=1)
segments(1, 1/8, 1, 4/8, lty=2)
text(2.6, 2.5/8, "\\scriptsize$\\smgil{P}(X = 1) =  F(1) - F(0)$")
par(opar)

#################### Figure 3.5 ##################

plot(1:6, 1:6, type = "n", axes = FALSE, xlab = "", ylab = "")
symbols(x = c(2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5), y = c(2.1, 2.3, 2.5, 2.7, 2.9, 3.1, 2.1, 2.3, 2.5, 2.7, 2.1, 2.3, 2.1), circles = rep(.1, 13), inches = FALSE, add = TRUE, bg = "skyblue3")
fulcrum <- mean(c(2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5))
points(fulcrum, 1.85, pch = 17, cex = 3)
segments(1.8, 2, 5.2, 2, lwd = 5)

#################### Example 3.18 ##################

#################### R Code 3.8 ##################

x <- c(1, 5, 30)
px <- c(0.5, 0.45, 0.05)
EX <- sum(x*px)
WM <- weighted.mean(x, px)
c(EX, WM)

#################### Example 3.19 ##################

#################### R Code 3.9 ##################

x <- c(1, 5, 30)
px <- c(0.5, 0.45, 0.05)
EgX <- sum((x - 5)*px)
WgM <- weighted.mean((x - 5), px)
c(EgX, WgM)


#################### Figure  3.6 ##################

par(mfrow = c(1, 3))
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
mtext(text="$\\gil{P}(a \\leq X \\leq b)$", side = 3, line = 2.5)
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
mtext(text="$\\gil{P}(X \\leq b)$", side = 3, line = 2.5)
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
mtext(text="$\\gil{P}(X \\leq a)$", side = 3, line = 2.5)
#
par(mfrow = c(1, 1))


#################### Example 3.21 ##################

#################### R Code 3.10  ##################

#################### Figure 3.7   ##################

opar <- par(no.readonly = TRUE)  # read current parameters
par(mfrow = c(1, 2), pty = "m")  # split device 1 row 2 columns
f <- function(x){
  y <- 3/4*(1 - x^2)
  y[x < -1 | x > 1] <- 0
  return(y)
}
curve(f, -2, 2, xlab = "$x$", ylab = "$f(x)$", main = "PDF for $X$")
#
F <- function(x){
  y <- -x^3/4 + 3*x/4 + 1/2
  y[x <= -1] <- 0
  y[x > 1] <- 1
  return(y)
}
curve(F, -2, 2, xlab = "$x$", ylab = "$F(x)$", main = "CDF for $X$")
par(opar)  # reset graphical parameters

#################### R Code 3.11 ##################

#################### Figure 3.8 ##################

library(ggplot2)
previous_theme <- theme_set(theme_bw()) # set black and white theme
p <- ggplot(data.frame(x = c(-2, 2)), aes(x = x))
p + stat_function(fun = f) + labs(x = "$x$", y = "$f(x)$\n", title = "PDF for $X$")
p + stat_function(fun = F) + labs(x = "$x$", y = "$F(x)$\n", title = "CDF for $X$")
theme_set(previous_theme) # Restore original theme


#################### Example 3.23 ##################

#################### R Code 3.12 ##################

fx <- function(x){3/4 - 3/4*x^2}           # define function fx
integrate(fx, lower = -0.5, upper = 1)     # gives value and tolerance
ans <- integrate(fx, lower = -0.5, upper = 1)$value   # just the value
ans
library(MASS)                              # for fractions() function
fractions(ans)                             # find closest fraction

#################### R Code 3.13 ##################

#################### Figure 3.9  ##################


f <- function(x){2*cos(2*x)}
curve(f, 0, pi/4, , xlab = "x", ylab = "2cos(2x)")
abline(v = pi/12, lty = 2, lwd = 2)
# ggplot2 now
p <- ggplot(data.frame(x = c(0, pi/4)), aes(x = x))
p + stat_function(fun = f) + labs(x = "x", y = "2cos(2x)") +
 geom_vline(xintercept = pi/12, lty = "dashed")
  
#################### Example 3.24 ##################

#################### R Code 3.14  ##################

#################### Figure 3.10  ##################


x <- seq(-1, 1, length = 500)
y <- dunif(x, -1, 1)
DF <- data.frame(fx = y)
previous_theme <- theme_set(theme_bw())  # set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) +
 geom_area(fill = "skyblue3") +
 labs(x = "x", y = "f(x)\n") +
 ylim(c(0, 1)) +
 theme_set(previous_theme)                # Restore original theme

#################### Example 3.25 ##################

#################### R Code 3.15  ##################


x <- 0:3
px <- c(1/8, 3/8, 3/8, 1/8)
EX <- weighted.mean(x, px)
EX2 <- weighted.mean(x^2, px)
VX <- EX2 - EX^2
sigmaX <- sqrt(VX)
MUSIG <- c(EX, VX)
names(MUSIG) <- c("E(X)", "V(X)")
MUSIG
Int <- 2*sigmaX*c(-1, 1) + 1.5
Int


#################### Figure 3.11 ##################

opar <- par(no.readonly = TRUE)  # read current parameters
par(mfrow = c(1, 3))  # split device 1 row 3 columns
x <- 0:10
px <- dbinom(x, size = 10, 0.8)
plot(x, px, type = "h", axes = FALSE, lwd = 2, main ="Negative Skew", xlab = "\\large$\\gamma_1 = -0.47434 < 0$", ylab = "")
abline(h=0, lwd=2)
px <- dbinom(x, size = 10, 0.5)
plot(x, px, type = "h", axes = FALSE, lwd = 2, main ="Symmetric", xlab = "\\large$\\gamma_1 = 0$", ylab = "")
abline(h=0, lwd=2)
px <- dbinom(x, size = 10, 0.2)
plot(x, px, type = "h", axes = FALSE, lwd = 2, main ="Positive Skew", xlab = "\\large$\\gamma_1 = 0.47434 > 0$", ylab = "")
abline(h=0, lwd=2)
par(opar)

#################### Example 3.26 ##################

#################### R Code 3.16  ##################

#################### Figure 3.12  ##################

 
x <- 1:5
px <- x/15
plot(x, px, xlab = "x", ylab = "P(X=x)", type = "h")
EX <- sum(x * px)
sigmaX <- sqrt(sum(x^2 * px) - EX^2)
X.star <- (x-EX)/sigmaX
skew <- sum(X.star^3 * px)
skew
 

