################### Chapter 02 Script
################### 7/23/15 V.1
############################## Chapter 2 ############################
library(PASWR2) 
library(vcd)

############################## Example 2.1 ##########################

Grades <- c("A", "D", "C", "D", "C", "C", "C", "C", "F", "B")
Grades
table(Grades)
xtabs(~Grades)
table(Grades)/length(Grades)      # Relative frequency table
# or
prop.table(table(Grades))


############################## Example 2.2 ##########################

library(MASS)
table(quine$Age)  # accessing Age using dollar prefixing
with(data = quine, table(Age))  
xtabs(~ Age, data = quine)


############################## Example 2.3 ##########################

############################## R Code 2.1  ##########################

opar <- par(no.readonly = TRUE)  # read in current parameters
par(mfrow=c(2, 2))               # change parameters
barplot(xtabs(~ Grades), col = "gray40", xlab = "Grades",
        ylab = "Frequency")
barplot(prop.table(xtabs(~ Grades)), col = "gray40", xlab = "Grades",
        ylab = "Relative Frequency")
barplot(xtabs(~ Age, data = quine), col = "gray90", xlab = "Age",
        ylab = "Frequency")
barplot(prop.table(xtabs(~ Age, data = quine)), col = "gray90",
        xlab = "Age", ylab = "Relative Frequency")
par(opar)                        # reset to original parameters


############################## Example 2.4 ##########################

opar <- par(no.readonly = TRUE)  # read in current parameters
par(mfrow=c(1, 2))
dotchart(xtabs(~ Grades), main = "Grades", bg = "gray40",
         xlim = c(0, 6))
dotchart(xtabs(~ Age, data = quine), main = "Age", bg = "gray60",
         xlim = c(25, 50))
par(opar)                        # reset to original parameters

############################## Example 2.5 ##########################

opar <- par(no.readonly = TRUE)    # read in current parameters
par(mfrow=c(1, 2))                 # one row two columns
TDM <- xtabs(Days ~ Age, data = quine)
dotchart(TDM, bg = "gray40", xlab = "Total Days Missed",
         xlim = c(400, 900))
ADM <- with(data = quine, tapply(Days, list(Age), mean))
dotchart(ADM, xlab = "Average Days Missed", bg = "gray60",
         xlim = c(10, 22))
par(opar)                          # reset to original parameters

############################## Example 2.6 ##########################


opar <- par(no.readonly = TRUE)          # read in current parameters
par(mfrow = c(1, 2))                     # one row two columns  
GS <- gray(c(0.1, 0.4, 0.7, 0.8, 0.95))  # different grays
pie(xtabs( ~ Grades), radius = 1, col = GS)
mtext("Grades", side = 3, cex = 1.25, line = 1)
pie(xtabs(~ Age, data = quine), radius = 1, col = GS)
mtext("Age", side = 3, cex = 1.25, line = 1)
par(opar)                                 # reset to original parameters    


############################## Figure 2.5 ##########################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow=c(3, 3), mar = c(1.1, 1.1, 2.1, 1.1)) 
curve(dnorm(x, -3.5, 1), -7, 7, axes = FALSE, xlab = "", ylab = "", n = 500, main = "1.  Bimodal")
curve(dnorm(x, 3.5, 1), -7, 7, add = TRUE, n = 500)
abline(h = 0) 
xs <- seq(0, 10, length = 500)
ys <- dgamma(xs, 2, 1)
plot(xs, ys, type = "l", axes = FALSE, main = "2.  Skew right", xlab = "", ylab = "")
abline(h = 0) 
curve(dbeta(x, 2, 2), 0, 1, axes = FALSE, main = "3.  Short tailed", n = 500, xlab = "", ylab = "")
abline(h = 0) 
curve(dunif(x, 0, 1), 0, 1,  axes = FALSE, main = "4.  Uniform", ylim =c(0, 1), xlab = "", ylab = "")
abline(h = 0)
lines(c(0, 0), c(0, 1))
lines(c(1, 1), c(0, 1)) 
curve(dnorm, -3.5, 3.5, axes = FALSE, xlab = "", ylab = "", main = "5.  Normal", n = 500)
abline(h = 0) 
plot(xs, axes = FALSE, xlim = c(0, 2), ylim = c(0, 1), type = "n", main = "6.  Triangular", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1))
lines(c(1, 2), c(1, 0))
abline(h = 0) 
curve(dt(x, 2), -6, 6, axes = FALSE, main = "7.  Long tailed", n = 500, xlab = "", ylab = "")
abline(h = 0) 
plot(-xs, ys, type = "l", axes = FALSE, main = "2.  Skew left", xlab = "", ylab = "")
abline(h = 0) 
curve(dnorm(x, -5.25, 0.5), -7, 7, axes = FALSE, xlab = "", ylab = "", n = 500, main = "9.  Multimodal")
curve(dnorm(x, -1.75, 0.5), -7, 7, add = TRUE, n = 500)
curve(dnorm(x, 1.75, 0.5), -7, 7, add = TRUE, n = 500)
curve(dnorm(x, 5.25, 0.5), -7, 7, add = TRUE, n = 500)
abline(h = 0) 
par(opar)

############################## Example 2.7 ##########################

############################## R Code  2.2 ##########################

NYYHR <- BABERUTH$hr[BABERUTH$team=="NY-A"]
NYYHR
stem(NYYHR)
rm(NYYHR)  # clean up


############################## Example 2.8 ##########################

############################## R Code  2.3 ##########################


head(BABERUTH)
NYYHR <- with(data=BABERUTH, hr[7:21])
stripchart(NYYHR, xlab="Home runs per season", method="stack",
           main="Strip chart of home runs while a New York Yankee", pch=1)
rm(NYYHR)  # clean up



############################## R Code  2.4 ##########################

opar <- par(no.readonly = TRUE)      # read in current parameters
par(mfrow = c(1, 2))
stripchart(hr ~ team, data = BABERUTH, xlab = "Home runs per season",
           pch = 1, method = "stack")
title("Strip chart of home runs \n by team")
par(las = 1)                        # Makes labels horizontal
stripchart(hr ~ team, data = BABERUTH, pch = 19, method = "stack",
           col = c("gray30","gray50","gray70"), xlab = "Home runs per season",
           main = "Grayscale strip chart of \n home runs by team")
par(opar)                           # reset to original parameters


############################## Example 2.9 ##########################

############################## R Code  2.5 ##########################

opar <- par(no.readonly = TRUE)  # read in current parameters
par(mfrow=c(1, 2))               # one row two columns
bin <- seq(20, 70, 10)           # creating bins 20-70 by 10
hist(BABERUTH$hr[7:21], breaks = bin, xlab = "Home Runs", col = "pink",
     main = "Bins of form (]")
hist(BABERUTH$hr[7:21], breaks = bin, right = FALSE, xlab = "Home Runs",
     col = "pink", main = "Bins of form [)")
par(opar)                        # reset to original parameters


############################## Example 2.10 ##########################

############################## R Code  2.6  ##########################

xs <- BABERUTH$hr[7:21]          # xs = home runs while NYY  
R <- diff(range(xs))             # R = range of data  
n <- length(xs)                  # number of observations in xs
hs <- R/(1 + log2(n))            # class interval width Sturges
hs
nclassS <- ceiling(R/hs)         # number of classes
nclassS
bpS <- min(xs) + hs*0:nclassS    # breakpoints using Definition
bpS
sturgesD <- hist(xs, breaks = bpS, main = "Sturges Definition", 
                 xlab = "", col = "pink")  # Histogram using Def
sturgesD$breaks                  # show breakpoints
sturgesD$counts                  # count in each bin
pretty(xs, n = nclassS)          # breakpoints using pretty
sturgesA <- hist(xs, breaks = "Sturges", main = "Sturges Adjusted", 
                 xlab = "", col = "blue")  # Histogram Adjusted
sturgesA$breaks                  # show adjusted breakpoints
sturgesA$counts                  # count in each bin

############################## R Code  2.7  ##########################

n <- length(xs)
hfd <- 2*IQR(xs)/(n^(1/3))        # class interval width FD
hfd
nclassFD <- ceiling(R/hfd)        # number of classes
nclassFD
bpFD <- min(xs) + hfd*0:nclassFD  # breakpoints using Definition
bpFD
FDdef <- hist(xs, breaks = bpFD, main = "FD Definition", xlab = "", 
              col = "pink")       # Histogram using Definition
FDdef$breaks                      # show breakpoints
FDdef$counts                      # count in each bin
pretty(xs, n = nclassFD)          # breakpoints using pretty
FDadj <- hist(xs, breaks = "FD", main = "FD Adjusted", xlab = "", 
              col = "blue")       # Histogram Adjusted
FDadj$breaks                      # show adjusted breakpoints
FDadj$counts                      # count in each bin

############################## R Code  2.8  ##########################

hsc <- 2*3^(1/3)*pi^(1/6)*sd(xs)/n^(1/3)  # class interval width Scott
hsc
nclassSC <- ceiling(R/hsc)                # number of classes
nclassSC
bpSC <- min(xs) + hsc*0:nclassSC          # breakpoints using Definition
scottD <- hist(xs, breaks = bpSC, main = "Scott Definition", xlab = "", 
               col = "pink")              # Histogram using Definition
scottD$breaks                             # show breakpoints
scottD$counts                             # count in each bin
pretty(xs, n = nclassSC)                  # breakpoints using pretty
scottA <- hist(xs, breaks = "Scott", main = "Scott Adjusted", 
               xlab = "", col = "blue") # Histogram Adjusted
scottA$breaks                             # show adjusted breakpoints
scottA$counts                             # count in each bin


############################## Figure  2.9  ##########################

opar <- par(no.readonly = TRUE)  # read in current parameters
par(mfrow = c(3, 2))             # Three rows, two columns
xs <- BABERUTH$hr[7:21]          # xs = home runs while NYY  
R <- diff(range(xs))             # R = range of data  
hs <- R/(1 + log2(length(xs)))   # class interval width Sturges
hs
nclassS <- ceiling(R/hs)         # number of classes
nclassS
binsS <- min(xs) + hs*0:nclassS  # breakpoints using Definition
std.hist <- hist(xs, breaks = binsS, main = "Sturges Definition", 
                 xlab = "", col = "pink")  # Histogram using Def
std.hist$breaks                  # show breakpoints
pretty(xs, n = nclassS)          # breakpoints using pretty
sta.hist <- hist(xs, breaks = "Sturges", main = "Sturges Adjusted", 
                 xlab = "", col = "blue")  # Histogram Adjusted
sta.hist$breaks                  # show adjusted breakpoints
n <- length(xs)
hfd <- 2*IQR(xs)/(n^(1/3))       # class interval width FD
hfd
nclassFD <- ceiling(R/hfd)       # number of classes
nclassFD
binsFD <- min(xs) + hfd*0:nclassFD  # breakpoints using Definition
std.hist <- hist(xs, breaks = binsFD, main = "FD Definition", 
                 xlab = "", col = "pink")  # Histogram using Def
std.hist$breaks                  # show breakpoints
pretty(xs, n = nclassFD)         # breakpoints using pretty
sta.hist <- hist(xs, breaks = "FD", main = "FD Adjusted", 
                 xlab = "", col = "blue")  # Histogram Adjusted
sta.hist$breaks                  # show adjusted breakpoints
hsc <- 2*3^(1/3)*pi^(1/6)*sd(xs)/n^(1/3)  # class interval width Scott
hsc
nclassSC <- ceiling(R/hsc)       # number of classes
nclassSC
binsSC <- min(xs) + hsc*0:nclassSC  # breakpoints using Definition
std.hist <- hist(xs, breaks = binsSC, main = "Scott Definition", 
                 xlab = "", col = "pink")  # Histogram using Def
std.hist$breaks                  # show breakpoints
pretty(xs, n = nclassSC)         # breakpoints using pretty
sta.hist <- hist(xs, breaks = "Scott", main = "Scott Adjusted", 
                 xlab = "", col = "blue")  # Histogram Adjusted
sta.hist$breaks                  # show adjusted breakpoints
par(opar)                        # reset to original parameters


############################## R Code  2.9  ##########################

opar <- par(no.readonly = TRUE)  # read in current parameters
par(mfrow=c(2, 2))               # two rows two columns
attach(geyser)                   # place geyser on search path
hist(duration, breaks = 3,  freq = FALSE, ylim = c(0, 1), col = "pink")
hist(duration, breaks = 6,  freq = FALSE, ylim = c(0, 1), col = "pink")
hist(duration, breaks = 12, freq = FALSE, ylim = c(0, 1), col = "pink")
hist(duration, breaks = 24, freq = FALSE, ylim = c(0, 1), col = "pink")
detach(geyser)                   # remove geyser from search path
par(opar)                        # reset to original parameters


############################## Figure  2.11  ##########################

h = 1
x <- seq(-3.3, 3.3, length = 500)
xi <- 0
n <- length(xi)
rect <- function(x){(abs(x) < 1)*0.5}
shapes <- sapply(xi, function(xi){(1/(h*n))*rect((x - xi)/h)})
fxr1 <- apply(shapes, 1, sum)
#
tri <- function(x){(abs(x) < 1)*(1 - abs(x))}
shapes <- sapply(xi, function(xi){(1/(h*n))*tri((x - xi)/h)})
fxt1 <- apply(shapes, 1, sum)
#
gauss <- function(x){1/sqrt(2*pi)*exp(-(x^2)/2)}
shapes <- sapply(xi, function(xi){(1/(h*n))*gauss((x - xi)/h)})
fxg1 <- apply(shapes, 1, sum)
#
h = 2
x <- seq(-3.3, 3.3, length = 500)
xi <- 0
rect <- function(x){(abs(x) < 1)*0.5}
shapes <- sapply(xi, function(xi){(1/(h*n))*rect((x - xi)/h)})
fxr2 <- apply(shapes, 1, sum)
#
tri <- function(x){(abs(x) < 1)*(1 - abs(x))}
shapes <- sapply(xi, function(xi){(1/(h*n))*tri((x - xi)/h)})
fxt2 <- apply(shapes, 1, sum)
#
gauss <- function(x){1/sqrt(2*pi)*exp(-(x^2)/2)}
shapes <- sapply(xi, function(xi){(1/(h*n))*gauss((x - xi)/h)})
fxg2 <- apply(shapes, 1, sum)
DF <- stack(list(rectangular = fxr1, triangular = fxt1, gaussian = fxg1, rectangular = fxr2, triangular = fxt2, gaussian = fxg2))
names(DF) <- c("kernel.value", "kernel.type")
head(DF)
DF$x = x
DF$h[1:1500] = "$h = 1$"
DF$h[1501:3000] = "$h = 2$"
head(DF)
library(ggplot2)
previous_theme <- theme_set(theme_bw()) # set black-and-white theme
p <- ggplot(data = DF, aes(x = x, y = kernel.value)) + geom_line() + facet_grid(kernel.type ~ h)
p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ylab("$K(x)$\n") + xlab("\n$x$")
theme_set(previous_theme) # Restore original theme


############################## R Code  2.10  ##########################

xi <- c(0.5, 1.0)   # assign values to xi
n <- length(xi)     # number of values
x <- seq(from = min(xi) - .5, to = max(xi) + .5, by = .1)
h <- 0.3
tri <- function(x){(abs(x) < 1)*(1 - abs(x))}
shapes <- sapply(xi, function(xi){(1/(h*n))*tri((x - xi)/h)})
fx <- apply(shapes, 1, sum)
plot(x, fx, type = "l", xlab = "x", ylab = expression(hat(f)(x)))


############################## R Code  2.11  ##########################

dimnames(shapes) <- list(x, c(xi[1], xi[2]))  # list
RES <- cbind(shapes, f = apply(shapes,1, sum))
RES


############################## Example 2.11  ##########################

############################## R Code  2.12  ##########################

xi <- c(2.1, 2.2, 2.3, 2.4, 2.6, 2.7, 3.2, 3.3, 3.6, 3.7)
n <- length(xi)
x <- seq(from = min(xi) - 1, to = max(xi) + 1, length.out = 800)
h <- 0.3
gauss <- function(x){1/sqrt(2*pi)*exp(-(x^2)/2)}
shapes <- sapply(xi, function(xi){(1/(h*n))*gauss((x - xi)/h)})
plot(x, apply(shapes, 1, sum), type = "l", ylab = "", lwd = 3)
rug(xi, lwd = 2)
apply(shapes, 2, function(b){lines(x, b)})


############################## Example 2.12  ##########################

############################## R Code  2.13  ##########################

opar <- par(no.readonly = TRUE)  # read in current parameters
par(mfrow=c(1, 2))               # make device region 1 by 2
with(data = geyser, 
     hist(waiting, freq = FALSE, col="grey95")
)     # density histogram
with(data = geyser, 
     lines(density(waiting), col="red", lwd=2)
)     # superimpose kernel density estimate over histogram
with(data = geyser, 
     plot(density(waiting), col="red", lwd=2,
          main="Density of waiting")
)     # create kernel density estimate by itself
par(opar)  # reset to original parameters

############################## Example 2.13  ##########################

NYYHR <- with(data=BABERUTH, hr[7:21])
NYYHR
SNYYHR <- sort(NYYHR)
SNYYHR
p.05 <- floor(.05*15)
p.10 <- floor(.10*15)
p.15 <- floor(.15*15)
p.50 <- floor(.50*15)
num.to.delete <-c(p.05, p.10, p.15, p.50)
num.to.delete
m.05 <- mean(SNYYHR[(1+p.05):(15-p.05)])
m.10 <- mean(SNYYHR[(1+p.10):(15-p.10)])
m.15 <- mean(SNYYHR[(1+p.15):(15-p.15)])
m.50 <- mean(SNYYHR[(1+p.50):(15-p.50)])
t.m <- c(m.05, m.10, m.15, m.50)
names(t.m) <- c("5%tmean","10%tmean","15%tmean","50%tmean")
t.m
tm.05 <- mean(NYYHR, trim=.05)
tm.10 <- mean(NYYHR, trim=.10)
tm.15 <- mean(NYYHR, trim=.15)
tm.50 <- mean(NYYHR, trim=.50)
tms <- c(tm.05, tm.10, tm.15, tm.50)
names(tms) <- c("5%tmean","10%tmean","15%tmean","50%tmean")
tms

############################## Example 2.14  ##########################

############################## R Code 2.14  ##########################

Student1 <- c(73, 75, 74, 74)
Student2 <- c(95, 94, 12, 95)
Student3 <- c(66, 67, 63, 100)
median(Student1)
median(Student2)
median(Student3)
SM <- rbind(Student1, Student2, Student3)  # combine rows
colnames(SM) <- c("Test1", "Test2", "Test3", "Test4")
SM
means <- apply(SM, 1, mean)                # mean of rows
medians <- apply(SM, 1, median)            # median of rows
TOT <- cbind(SM, means, medians)           # combine columns
TOT

############################## Example 2.15  ##########################

Grades <- c("A", "D", "C", "D", "C", "C", "C", "C", "F", "B")
table(Grades)
names(which.max(table(Grades)))


############################## Example 2.16  ##########################

############################## R Code 2.15   ##########################

plot(density(VIT2005$totalprice), main = "")

############################## R Code 2.16   ##########################

DV <- density(VIT2005$totalprice)  
yval <- max(DV$y)
ID <- which(DV$y == yval)
MODE <- DV$x[ID]
MODE

############################## Example 2.17  ##########################

############################## R Code 2.17   ##########################


x <- c(1, 4, 7, 9, 10, 14, 15, 16, 20, 21)
p <- c(0.25, 0.5, 0.75)                # desired quantiles
n <- length(x)                         # number of values, n
order.stat <- p*(n - 1) + 1            # computing order statistics
order.stat                             # order statistics
Q1 <- x[3] + 0.25*(x[4] - x[3])        # linear interpolation
Q2 <- x[5] + 0.50*(x[6] - x[5])        # linear interpolation
Q3 <- x[7] + 0.75*(x[8] - x[7])        # linear interpolation
QU <- c(Q1, Q2, Q3)
names(QU) <- c("Q1", "Q2", "Q3")
QU                                     # quartiles
quantile(x, probs=c(0.25, 0.5, 0.75))  # the easy way!

############################## Example 2.18  ##########################

NYYRBI <- with(data=BABERUTH, rbi[7:21])  # Extract RBIs while a NYY
SNYYRBI <- sort(NYYRBI)
p <- c(0.25, 0.50, 0.75)
n <- length(NYYRBI)
order.stat <- p*(n - 1) + 1
order.stat
Q1 <- SNYYRBI[4] + 0.5*(SNYYRBI[5] - SNYYRBI[4])
Q2 <- SNYYRBI[8]
Q3 <- SNYYRBI[11] + 0.5*(SNYYRBI[12] - SNYYRBI[11])
QU <- c(Q1, Q2, Q3)
names(QU) <- c("Q1", "Q2", "Q3")
QU
quantile(NYYRBI, probs=c(0.25, 0.50, 0.75))
j <- (floor((n + 1)/2) + 1)/2              # Number to count in
j
lower.hinge <- SNYYRBI[4] + 0.5*(SNYYRBI[5] - SNYYRBI[4])
upper.hinge <- SNYYRBI[11] + 0.5*(SNYYRBI[12] - SNYYRBI[11])
small <- min(NYYRBI)
large <- max(NYYRBI)
five.numbers <- c(small, lower.hinge, Q2, upper.hinge, large)
five.numbers
fivenum(NYYRBI)                


############################## Figure 2.16 ##########################


set.seed(13)
boxplot(BODYFAT$fat, horizontal=TRUE, ylim=c(6,49))
f <- fivenum(BODYFAT$fat)
HS <- f[4] - f[2]
Fl <- f[2] - 1.5*HS
Fu <- f[4] + 1.5*HS
# text(f, rep(1.5, 5), labels=c(expression(Min[]), expression(H[L]),
# expression(Q[2]), expression(H[U]), expression(Max[])) )
text(f, rep(1.5, 5), labels = c("$Min$", "$H_L$", "$Q_2$", "$H_U$", "$Max$"))
# text(c(Fl,Fu), c(1.5, 1.5), labels=c(expression(Fence[L]), expression(Fence[U])))
text(c(Fl,Fu), c(1.5, 1.5), labels=c("$F_L$", "$F_U$"))
points(BODYFAT$fat, jitter(rep(1.35,length(BODYFAT$fat)), factor=1), pch=19, col="gray20")
lines(c(f[1],f[1]), c(1,1.45), lty=2)
lines(c(f[2],f[2]), c(0.55,1.45), lty=2)
lines(c(f[4],f[4]), c(0.55,1.45), lty=2)
lines(c(f[5],f[5]), c(1,1.45), lty=2)
lines(c(Fl, Fl), c(0.55, 1.45), lty=2)
lines(c(Fu, Fu), c(0.55, 1.45), lty=2)
arrows(Fl+.5, 0.55, f[2]-.5, 0.55, length = 0.1, angle = 30, code = 3)
arrows(f[4]+.5, 0.55, Fu-.5, 0.55, length = 0.1, angle = 30, code = 3)
arrows(f[2]+.5, 0.55, f[4]-.5, 0.55, length = 0.1, angle = 30, code = 3)
# text( c(10, 20, 30, 40), rep(0.50,4), labels=c(expression(Outliers[]),
# expression(1.5*H[spread]),  expression(H[spread]), expression(1.5*H[spread])) )
text( c(10, 20, 30, 40), rep(0.50,4), labels=c("Outliers", "$1.5\\times H_{spread}$", "$H_{spread}$", "$1.5\\times H_{spread}$"))


############################## Figure 2.17 ##########################

library(MASS)                           # load MASS package
opar <- par(no.readonly = TRUE)         # read in current parameters
par(mar=c(0.1, 7.1, 0.1, 0.1))          # more space needed on side 2
with(data = Cars93, boxplot(Min.Price, ylim = c(0, 50),
                            ylab = "Minimum Price (in \\$1000)\n for basic version",
                            col = "springgreen3"))
f <- with(data=Cars93, fivenum(Min.Price))  # store fivenum values in f
text(x = rep(1.25, 5), y = f, labels=c("Min", expression(H[L]),
                                       expression(Q[2]) , expression(H[U]), "Max"), pos=4)
par(opar)                               # reset to original parameters


############################## Example 2.19 ##########################

############################## R Code 2.18 ##########################

library(MASS)                           # load MASS package
opar <- par(no.readonly = TRUE)         # read in current parameters
par(mar=c(0.1, 7.1, 0.1, 0.1))          # more space needed on side 2  
with(data = Cars93, boxplot(Min.Price, ylim = c(0, 50), 
                            ylab = "Minimum Price (in \\$1000)\n for basic version", 
                            col = "springgreen3"))
f <- with(data=Cars93, fivenum(Min.Price))  # store fivenum values in f
text(x = rep(1.25, 5), y = f, labels=c("Min", expression(H[L]),
                                       expression(Q[2]) , expression(H[U]), "Max"), pos=4)
par(opar)                               # reset to original parameters



############################## Example 2.20 ##########################

############################## R Code 2.19  ##########################

BODYFAT$sex <- factor(BODYFAT$sex, labels = c("Female", "Male"))
boxplot(fat ~ sex, data = BODYFAT, col = c("gray80", "gray20"),
        ylab = "Percent Bodyfat")


############################## 2.6.1 Range ##########################

range(1:10)
diff(range(1:10))

############################## 2.6.2 Interquantile Range #####################

quantile(1:10)
IQR(1:10)

############################## R Code 2.20 #############################

x <- 1:5
n <- length(x)
mean.x <- mean(x)
mean.x
x-mean.x
(x-mean.x)^2
NUM <- sum((x-mean.x)^2)  # numerator of s^2 hard way
NUM
DEN <- n-1                # denominator of s^2
DEN
VAR <- NUM/DEN            # variance hard way
VAR
var(x)                    # variance easy way
SD <- sqrt(VAR)           # standard deviation hard way
SD
sd(x)                     # standard deviation with R

summary(x)

############################## Example 2.21 ##########################

meanSEC <- mean(SDS4$times)
meanMIN <- mean(SDS4$times/60)
c(meanSEC, meanMIN)


sdSEC <- sd(SDS4$times)
sdMIN <- sd(SDS4$times/60)
c(sdSEC, sdMIN)
CVsec <- meanSEC/sdSEC
CVmin <- meanMIN/sdMIN
c(CVsec, CVmin)

############################## Example 2.22 ##########################

rsd <- function(x){
  abs(sd(x)/mean(x))*100
}
ANS <- tapply(MILKCARTON$seconds, MILKCARTON$size, rsd)
ANS

############################## Example 2.23 ##########################

median(abs(SDS4$times - median(SDS4$times)))
mad(SDS4$times, constant = 1) 


############################## Table 2.3 in Latex ##########################

library(xtable)
EPIDURAL$ease <- factor(EPIDURAL$ease, levels = c("Easy", "Difficult", "Impossible"))
# Levels of Ease in increasing order 
DT <- xtabs(~doctor + ease, data = EPIDURAL)   
ST <- xtable(DT, align = 'cccc', digits = 0, caption = 'Two-way table of \\type{Doctor} by \\type{Ease}', label = 'DoctorEase')
print(ST, include.rownames = TRUE, caption.placement = "top", booktabs = TRUE)
rm(EPIDURAL)

############################## Example 2.24 ##########################

############################## R Code 2.21 ###########################


head(EPIDURAL)                          # First six rows of EPIDURAL
xtabs(~doctor + ease, data = EPIDURAL)  # levels listed alphabetically
EPIDURAL$ease <- factor(EPIDURAL$ease, levels = c("Easy", "Difficult", 
                                                  "Impossible"))  # levels in order of difficulty
xtabs(~doctor + ease, data = EPIDURAL)  # levels in proper order  

############################## R Code 2.22 ###########################

with(data = EPIDURAL, ftable(doctor, treatment, ease))


############################## Example 2.25 ###########################

############################## R Code 2.23  ###########################

X <- xtabs(~doctor + ease, data = EPIDURAL)
X
t(X)      # Transpose X

############################## R Code 2.24 ###########################

opar <- par(no.readonly = TRUE)  # read in current parameters
par(mfrow=c(2, 2))               # 2 rows and 2 columns
barplot(X)                       # top left graph
title("Doctor Stacked within \n Levels of Palpation")
barplot(t(X))                    # top right graph
title("Levels of Palpation \n Stacked within Doctor")
barplot(X, beside = TRUE)        # bottom left graph
title("Doctor Grouped within \n Levels of Palpation")
barplot(t(X), beside = TRUE)     # bottom right graph
title("Levels of Palpation \n Grouped within Doctor")
par(opar)                        # rest original parameters


############################## Example 2.26 ###########################

############################## R Code 2.25  ###########################

xtabs(~treatment + oc, data = EPIDURAL)
addmargins(xtabs(~treatment + oc, data = EPIDURAL))  # addmargins
X <- prop.table(xtabs(~treatment + oc, data = EPIDURAL), 1)  
X  # Percents by rows

############################## R Code 2.26  ###########################

barplot(X, beside=TRUE, legend=TRUE, ylim= c(0, 0.5))


############################## R Code 2.27  ###########################

barplot(t(X), beside=TRUE, legend=TRUE, ylim = c(0, 0.5))




############################## Example 2.27  ###########################

############################## R Code 2.28   ###########################

opar <- par(no.readonly = TRUE)          # read in current parameters
par(mfrow=c(2, 2))                       # 2*2 plotting region
EPIDURAL$BMI <- EPIDURAL$kg/(EPIDURAL$cm/100)^2     # Create BMI variable
with(data = EPIDURAL,  hist(BMI[treatment == "Traditional Sitting"], 
                            xlab = "BMI", main = "Sitting", col = 'gray70'))   # top left
with(data = EPIDURAL, hist(BMI[treatment == "Traditional Sitting"], 
                           xlim = c(20, 60), ylim = c(0, 17), xlab = "BMI", main = "Sitting", 
                           col = 'gray30'))                                   # top right
with(data = EPIDURAL, hist(BMI[treatment == "Hamstring Stretch"], 
                           xlab = "BMI", main = "Hamstring", col = 'gray70')) # bottom left
with(data = EPIDURAL, hist(BMI[treatment == "Hamstring Stretch"], 
                           xlim = c(20, 60), ylim = c(0, 17), xlab = "BMI", main = "Hamstring", 
                           col = 'gray30'))                                   # bottom right
par(opar)                                # reset to original parameters


############################## Example 2.28   ###########################

############################## R Code 2.29    ###########################

with(data=EPIDURAL, boxplot(BMI ~ treatment, horizontal=TRUE))


############################## R Code 2.30    ###########################

BMITS <- with(data = EPIDURAL, BMI[treatment == "Traditional Sitting"])
BMIHS <- with(data = EPIDURAL, BMI[treatment == "Hamstring Stretch"])
plot(density(BMITS), xlim = c(20, 60), lwd = 2, main = "",  xlab = "BMI")
lines(density(BMIHS), lty = 2, lwd = 2)
rm(BMITS, BMIHS)  # clean up


############################## Example 2.29    ###########################

############################## R Code 2.31     ###########################

BMITS <- with(data = EPIDURAL, BMI[treatment == "Traditional Sitting"])
BMIHS <- with(data = EPIDURAL, BMI[treatment == "Hamstring Stretch"])
qqplot(x = BMITS, y = BMIHS, xlim = c(20, 60), ylim = c(20, 60),
       xlab = "Traditional Sitting", ylab = "Hamstring Stretch")
abline(a = 0, b = 1)    # Line y = 0 + 1*x
rm(BMITS, BMIHS)        # clean up

############################## Example 2.30    ###########################

library(MASS)
range(Animals$body)
range(Animals$brain)
range(log(Animals$body))
range(log(Animals$brain))
with(data = Animals, plot(body, brain, log = 'xy'))


############################## R Code 2.32    ###########################

with(data = Animals, plot(body, brain, log = 'xy'))
with(data = Animals,
     identify(body, brain, log = "xy", labels = row.names(Animals)) )


############################## Example 2.31    ###########################

library(MASS)
logbody <- log(Animals$body)
logbrain <- log(Animals$brain)
Zbody <- (logbody - mean(logbody))/sd(logbody)
Zbrain <- (logbrain - mean(logbrain))/sd(logbrain)
Anim <- cbind(Animals, logbody, logbrain, Zbody, Zbrain)
n <- sum(!is.na(logbody))
r <- (1/(n-1))*sum(Zbody*Zbrain)  # Definition of r
r

cor(logbody, logbrain)
head(Anim)                # Show first 6 rows of Anim


############################## R Code 2.33    ###########################


ZBO <- scale(logbody)               # Z score of logbody
dimnames(ZBO) <- list(NULL, "ZLBO") # name the column ZLBO
ZBR <- scale(logbrain)              # Z score of logbrain
dimnames(ZBR) <- list(NULL, "ZLBR") # name the column ZLBR
SAME <- cbind(Zbody, ZBO, Zbrain, ZBR)
SAME[1:5,]                          # Show first five rows of data frame


############################## Example 2.32    ###########################

############################## R Code 2.34    ###########################

library(MASS)
CWD <- with(data = Animals, 
            cor(log(body), log(brain)))  # Correlation with dinosaurs
CWD
SA <- Animals[order(Animals$body), ]     # Sorted by body weight
tail(SA, n = 4)  # Equivalently SA[25:28, ], shows four heaviest animals
NoDINO <- SA[-(28:26), ]                 # Remove rows 26-28 of SA
NoDINO[22:25, ]                          # Show four heaviest animals
CND <- with(data = NoDINO, 
            cor(log(body), log(brain)))  # Correlation without dinosaurs
CND


############################## Figure 2.27 ####################################

DT <- data.frame(x=c(1,2,3,4,5), Y=c(2,1,4,3,5))
plot(DT$x, DT$Y, pch=19, xlab="$x$", ylab="$Y$", xlim=c(0, 6), ylim=c(0, 6),
     las=0, cex=1.5)
model <- lm(Y~x, data=DT)
abline(model, lwd=2)
yhat <- fitted(model)
segments(DT$x[1], yhat[1], DT$x[1], DT$Y[1], lty=3)
segments(DT$x[2], yhat[2], DT$x[2], DT$Y[2], lty=3)
segments(DT$x[3], yhat[3], DT$x[3], DT$Y[3], lty=3)
segments(DT$x[4], yhat[4], DT$x[4], DT$Y[4], lty=3)
segments(DT$x[5], yhat[5], DT$x[5], DT$Y[5], lty=3)
points(DT$x[1:5], yhat, pch=22, bg="white", cex=1.5)
arrows(2, 0.4, 2, 1-.1, length=0.1, lwd=2)
arrows(1.7, 0.4, 2, 0.4, length=0, lwd=2)
arrows(2, 3, 2, yhat[2]+.1, length=0.1, lwd=2)
arrows(1.7, 3, 2, 3, length=0, lwd=2)
text(0.8, 1.7, "$\\hat{\\varepsilon}_1$")
text(1.5, 3, "$\\widehat{Y}_2$")
text(1.5, 0.4, "$Y_2$")
text(3, 1.6, "$\\left. \\rule{0pt}{4.5ex} \\right\\} \\hat{\\varepsilon}_2 = Y_2 -\\widehat{Y}_2$")
text(2.8, 3.5,"$\\hat{\\varepsilon}_3$")
text(3.8, 3.3,"$\\hat{\\varepsilon}_4$")
text(4.8, 4.8,"$\\hat{\\varepsilon}_5$")

############################## Example 2.33    ###########################

############################## R Code 2.35    ###########################

Y <- log(Animals$brain)
X <- log(Animals$body)
plot(X, Y, xlab = "log(body)", ylab = "log(brain)")
b1 <- sum((X - mean(X))*(Y - mean(Y)))/sum((X - mean(X))^2)
b0 <- mean(Y) - b1*mean(X)
estimates <- c(b0, b1)
estimates
modDINO <- lm(Y ~ X)
modDINO
abline(modDINO, col = "pink", lwd = 2)
SA <- Animals[order(Animals$body),]    # Sorted by body weight
NoDINO <- SA[-(28:26), ]               # Remove rows 26-28 (dinosaurs)
Y <- log(NoDINO$brain)
X <- log(NoDINO$body)
b1 <- sum((X - mean(X))*(Y - mean(Y)))/sum((X - mean(X))^2)
b0 <- mean(Y) - b1*mean(X)
estimates <- c(b0, b1)
estimates
modNODINO <- lm(Y ~ X)
modNODINO
abline(modNODINO, col = "blue", lwd = 2, lty = 2)
leglab <- c("OLS with Dinosaurs", "OLS without Dinosaurs")
leglty <- c(1, 2)
legcol=c("pink","blue")
legend("bottomright", legend=leglab, lty=leglty, col=legcol, lwd=2)



############################## Example 2.34    ###########################

############################## R Code 2.36     ###########################

plot(log(Animals$body), log(Animals$brain), col="blue", 
     xlab = "log(body)", ylab = "log(brain)")
f <- log(Animals$brain) ~ log(Animals$body)
modelLM <- lm(f)
abline(modelLM, col = "pink", lwd = 2)
modelLQS <- lqs(f)
abline(modelLQS, lty = 2, col = "red", lwd = 2)
modelRLM <- rlm(f, method = "MM")
abline(modelRLM, lty = 3, col = "black", lwd = 2)
leglabels <- c("Least Squares Line","Least-Trimmed Squares", 
               "Robust Line: M-estimator ")
leglty <- c(1, 2, 3)
legend("bottomright", legend = leglabels, lty = leglty, 
       col = c("pink","red","black"), lwd = 2, cex = 0.85)



############################## Example 2.35    ###########################

############################## R Code 2.37     ###########################

mat33 <- matrix(1:9, byrow = TRUE, nrow = 3)
mat33
layout(mat33)   # split the device according to mat33
layout.show(9)  # show the nine plots


############################## R Code 2.38     ###########################

mat32 <- matrix(c(1, 1, 2, 2, 3, 3), byrow = TRUE, nrow = 3)
mat32
layout(mat32)
layout.show(3)

############################## R Code 2.39     ###########################

mat35 <- matrix(c(1, 1, 1, 2, 3, 1, 1, 1, 4, 5, 6, 6, 7, 7, 7), 
                byrow = TRUE, nrow = 3)
mat35
layout(mat35)
layout.show(7)



############################## Example 2.36    ###########################

############################## R Code 2.40     ###########################


opar <- par(no.readonly = TRUE)  # copy of current settings
mat44 <- matrix(c(1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2, 3, 3, 3, 0), 
                byrow = TRUE, nrow = 4) 
mat44
layout(mat44)                    # split graphics device
par(mar = c(4, 4, 4, 4))
plot(hwfat ~ tanfat, col = "red", pch = 19, main = "", data = HSWRESTLER)
boxplot(HSWRESTLER$hwfat, col = "blue", ylab = "hwfat")
boxplot(HSWRESTLER$tanfat, col = "purple", horizontal = TRUE, 
        xlab = "tanfat")
par(opar)                        # restore original settings


############################## R Code 2.41     ###########################

levels(EPIDURALF$ease)
EPIDURALF$ease <- factor(EPIDURALF$ease, levels = c("Easy", "Difficult", 
                                                    "Impossible"))
levels(EPIDURALF$ease)

T1 <- xtabs(~ doctor, data = EPIDURALF)
T1
addmargins(T1)  # add margins to T1
prop.table(T1)  # Fraction of patients each physician treated


############################## R Code 2.42     ###########################

T2 <- xtabs(~ doctor + ease, data = EPIDURALF)
addmargins(T2)  # add margins
prop.table(T2, 1)  # compute fraction going across rows

############################## R Code 2.43     ###########################

library(vcd)  # load vcd package
mosaic(~ doctor, data = EPIDURALF)  # left plot
mosaic(~ doctor + ease, data = EPIDURALF)  # right plot

############################## R Code 2.44     ###########################

mosaic(~ doctor + ease, data = EPIDURALF, highlighting_fill = c("gray80", "gray50", "gray20"), highlighting = "ease")

############################## R Code 2.45     ###########################

mosaic(~ doctor + ease, data = EPIDURALF, shade = TRUE)



############################## Example 2.37     ###########################

############################## R Code 2.46     ###########################

UCB <- as.data.frame(UCBAdmissions)
prop.table(xtabs(Freq ~ Gender + Admit, data = UCB), 1)

mosaic(~ Gender + Admit, data = UCB, shade = TRUE)


############################## R Code 2.47     ###########################

prop.table(xtabs(Freq ~ Admit + Gender + Dept, data = UCB), c(2, 3))


############################## R Code 2.48     ###########################

UCB$Admit <- factor(UCB$Admit, labels = c("Yes", "No"))
mosaic( ~ Dept + Gender + Admit, data = UCB, direction = c("v", "h", "v"), highlighting = "Admit", highlighting_fill=c("gray80", "gray20"))

############################## Example 2.38     ###########################

EPIDURAL$BMI <- EPIDURAL$kg/(EPIDURAL$cm/100)^2  # create BMI variable
library(lattice)                                 # load lattice package
histogram(~BMI|treatment, data = EPIDURAL, layout = c(1, 2))


############################## Example 2.39     ###########################

############################## R Code  2.49     ###########################


bwplot(treatment~BMI|doctor, data=EPIDURAL, as.table=TRUE)


############################## R Code  2.50     ###########################

xtabs(~treatment + doctor, data = EPIDURAL)

############################## R Code  2.51     ###########################

stripplot(treatment~BMI|doctor, jitter=TRUE, data=EPIDURAL, as.table=TRUE)


############################## 2.9.3 Arranging Several Lattice ....  ###########################

print(latticegraph, split=c(column, row, number_of_columns,
                            number_of_rows), more=TRUE/FALSE)

print(latticegraph, position=c(x_LL, y_LL, x_UR, y_UR), more=TRUE/FALSE)

############################## Example 2.40     ###########################

############################## R Code  2.52     ###########################


graph1 <- histogram(~BMI, data = EPIDURAL)
graph2 <- xyplot(cm ~ kg|doctor, data = EPIDURAL, as.table = TRUE)
graph3 <- densityplot(~BMI|treatment, data = EPIDURAL, as.table = TRUE)
graph4 <- bwplot(~BMI|doctor, data = EPIDURAL, as.table = TRUE)
print(graph1, split=c(1, 2, 2, 2), more = TRUE)   # Lower left
print(graph2, split=c(2, 2, 2, 2), more = TRUE)   # Lower right
print(graph3, split=c(1, 1, 2, 2), more = TRUE)   # Upper left
print(graph4, split=c(2, 1, 2, 2), more = FALSE)  # Upper right

############################## R Code  2.53     ###########################

print(graph1, position=c(0, 0, 0.5, 0.5), more = TRUE)   # Lower left
print(graph2, position=c(0.5, 0, 1, 0.5), more = TRUE)   # Lower right
print(graph3, position=c(0, 0.5, 0.5, 1), more = TRUE)   # Upper left
print(graph4, position=c(0.5, 0.5, 1, 1), more = FALSE)  # Upper right


############################## Example  2.41     ###########################

############################## R Code  2.54     ###########################


library(MASS)                                          # Needed for lqs
xyplot(cm ~ kg|doctor, data = EPIDURAL, as.table = TRUE,
       panel=function(x, y) {
         panel.xyplot(x, y)                                   # x-y plot
         panel.abline(lm(y ~ x))                              # lm line
         panel.abline(lqs(y ~ x), col = 3, lty = 2, lwd = 2)  # lqs line
       })

############################## R Code  2.55     ###########################

panel.scatreg <- function(x, y)                        # name function
{
  panel.xyplot(x, y)                                   # make x-y plot
  panel.abline(lm(y ~ x), lwd = 2)                     # lm line
  panel.abline(lqs(y ~ x), col = 3, lty = 2, lwd = 2)  # lqs line
}
xyplot(cm~kg|doctor, data = EPIDURAL, as.table = TRUE, 
       panel = panel.scatreg)

############################## R Code  2.56     ###########################

previous_theme <- theme_set(theme_bw())       # set black-and-white theme
p <- ggplot(data = EPIDURALF)                 # Empty plot
p1 <- p + geom_boxplot(aes(x = treatment, y = kg, fill = treatment)) + 
  guides(fill = FALSE) + facet_grid(doctor ~ .)  
p1                                            # Left plot
p2 <- p1 + scale_fill_grey(start = .4, end = .6) 
p2                                            # Right plot
theme_set(previous_theme)                     # Restore original theme


############################## R Code  2.57     ###########################

previous_theme <- theme_set(theme_bw())   # set black-and-white theme
p <- ggplot(data = EPIDURALF, aes(x = treatment, y = kg, 
                                  fill = treatment)) # Empty plot
p1 <- p + geom_boxplot() + scale_fill_grey(start = .3, end = .7)  
p1                                                   # Left plot
p2 <- p1 + guides(fill = FALSE)                      # removes legend
p2                                                   # Center plot
p3 <- p2 + facet_grid(doctor ~ .)         # splits boxplot by physician 
p3                                        # Right plot
theme_set(previous_theme)                 # Restore original theme


############################## R Code  2.58     ###########################

previous_theme <- theme_set(theme_bw())    # set black-and-white theme
p <- ggplot(data = EPIDURALF, aes(x = kg)) # Empty plot
p1 <- p + geom_density()
p1                                         # density plot of kg
p2 <- p1 + facet_grid(treatment ~ .)
p2                # density plot of kg split by treatment
p3 <- p1 + facet_grid(treatment ~ doctor)
p3               # density plot of kg split by treatment and physician
theme_set(previous_theme)                  # Restore original theme


############################## Example2.42    ###########################

############################## R Code  2.59   ###########################

previous_theme <- theme_set(theme_bw())     # set black-and-white theme
p <- ggplot(data = EPIDURALF, aes(x = cm, y = kg))  # Empty plot 
p1 <- p + geom_point(aes(color = ease)) + scale_color_grey()
p1                                          # Left scatterplot
p2 <- p + geom_point(aes(shape = ease)) 
p2                                          # Right scatterplot
theme_set(previous_theme)                   # Restore original theme


############################## R Code  2.60   ###########################

previous_theme <- theme_set(theme_bw())    # set black-and-white theme
p <- ggplot(data = EPIDURALF, aes(x = cm, y = kg, color = ease, 
                                  shape = ease))  # Empty plot  
TEXT <- "Ease of\nPalpating\nPatient"      # \n returns a new line
p1 <- p + geom_point() + scale_color_grey()  
p1                                         # Left plot
p2 <- p1 + guides(color = guide_legend(TEXT), 
                  shape = guide_legend(TEXT)) + labs(x = "Height (cm)", 
                                                     y = "Weight (kg)")  
p2                                         # Right plot
theme_set(previous_theme)                  # Restore original theme


############################## R Code  2.61   ###########################

previous_theme <- theme_set(theme_bw())     # set black-and-white theme
p <- ggplot(data = EPIDURALF, aes(x = kg))  # Empty plot
p1 <- p + geom_histogram(binwidth = 10)  
p1                                          # Left histogram
p2 <- p + geom_histogram(binwidth = 5, fill = "gray65") 
p2                                          # Center histogram 
p3 <- p + geom_histogram(aes(y = ..density..), binwidth = 5, 
                         fill = "gray65") +  geom_density() + 
  labs(x = "Weight (kg)", y = "density")  
p3                                          # Right histogram
theme_set(previous_theme)                   # Restore original theme


############################## R Code  2.62   ###########################

previous_theme <- theme_set(theme_bw())      # set black-and-white theme
p <- ggplot(data = EPIDURALF, aes(x = ease)) # Empty plot
p1 <- p + geom_bar(position = "stack") + 
  theme(axis.text.x  = element_text(angle = 75, vjust = 0.5))
p1                                           # Left barplot
p2 <- p + geom_bar(aes(fill = doctor), position = "stack") + 
  scale_fill_grey() +
  theme(axis.text.x  = element_text(angle = 75, vjust = 0.5))
p2                                           # Center stacked barplot
p3 <- p + geom_bar(aes(fill = doctor), position = "dodge") +  
  scale_fill_grey() +
  theme(axis.text.x  = element_text(angle = 75, vjust = 0.5))
p3                                           # Right dodged barplot 
theme_set(previous_theme)                    # Restore original theme


############################## R Code  2.63   ########################### 

previous_theme <- theme_set(theme_bw())     # set black-and-white theme
p <- ggplot(data = EPIDURALF, aes(x = doctor, fill = ease))  # empty plot
p1 <- p  + geom_bar(position = "fill")  + 
  labs(y ="") + 
  scale_fill_grey()  
p1                                             # top left barplot
p2 <- p1 + guides(fill = guide_legend("Ease of\nPalpating\nPatient")) + 
  labs(y = "")  + 
  facet_grid(treatment ~ .)
p2                                             # top right barplot
pn <- ggplot(data = EPIDURALF, aes(x = treatment, fill = ease)) 

p3 <- pn + geom_bar(position = 'fill') + facet_grid(doctor ~ .) + 
  labs(y = "", x = "") + 
  guides(fill = guide_legend("Ease of\nPalpating\nPatient")) + 
  scale_fill_grey()
p3                                             # bottom left barplot 
EPIDURALF$treatment <- factor(EPIDURALF$treatment, 
                              labels = c("HS", "TS")) # shorten labels
p4 <- ggplot(data = EPIDURALF, aes(x = treatment, fill = ease)) + 
  geom_bar(position = 'fill') + facet_grid(. ~ doctor) + 
  labs(y = "", x = "") + 
  guides(fill = guide_legend("Ease of\nPalpating\nPatient")) + 
  scale_fill_grey()
p4                                              # bottom right barplot
EPIDURALF$treatment <- factor(EPIDURALF$treatment, 
                              labels = c("Hamstring Stretch", "Traditional Sitting"))  # reset
theme_set(previous_theme)                       # restore original theme


############################## R Code  2.64   ########################### 

previous_theme <- theme_set(theme_bw())   # set black-and-white theme
EPIDURALF$BMI <- EPIDURALF$kg/(EPIDURALF$cm/100)^2  # Create BMI 
p <- ggplot(data = EPIDURALF, aes(x = BMI, fill = ease)) # Empty plot
p1 <- p + geom_density(alpha = .2) + labs(x = "Body Mass Index") + guides(fill = guide_legend("Ease of\nPalpating\nPatient")) + scale_fill_grey()
p1  # Left density plots
p2 <- p1 + facet_grid(treatment~.)
p2  # Right density plots
theme_set(previous_theme)  # Restore original theme


############################## R Code  2.65   ########################### 

previous_theme <- theme_set(theme_bw())   # set black-and-white theme
DF <- data.frame(x = c(0, 1, 2), y = c(3, 0, 4))
p <- ggplot(data = DF, aes(x = x, y = y))
p + geom_area() 
p + geom_polygon()
p + geom_polygon(data = rbind(c(0, 0), DF, c(2, 0)))
theme_set(previous_theme)                 # Restore original theme


############################## R Code  2.66   ########################### 

previous_theme <- theme_set(theme_bw())   # set black-and-white theme
EPIDURALF$BMI <- EPIDURALF$kg/(EPIDURALF$cm/100)^2  # Create BMI 
dens <- density(EPIDURALF$BMI)
df.dens <- data.frame(x = dens$x, y = dens$y)
p <- ggplot(data = EPIDURALF, aes(x = BMI)) + 
  geom_density(fill = "gray", alpha = 0.4)
p1 <- p + geom_area(data = subset(df.dens, x >= 40 & 
                                    x <= max(EPIDURALF$BMI)), aes(x = x, y = y)) + 
  labs(x = "Body Mass Index", y = "", title = "geom\\_area()")
p1  # Left density plot
p2 <- p + geom_polygon(data = rbind(c(min(df.dens$x[df.dens$x >= 40]),0), 
                                    subset(df.dens, x >= 40 & x <= max(EPIDURALF$BMI)), 
                                    c(max(EPIDURALF$BMI), 0)), aes(x = x, y = y)) + 
  labs(y = "", x = "Body Mass Index",  title = "geom\\_polygon()")
p2  # Right density plot
theme_set(previous_theme)  # Restore original theme


############################## R Code  2.67   ########################### 



previous_theme <- theme_set(theme_bw())   # set black-and-white theme
EPIDURALF$BMI <- EPIDURALF$kg/(EPIDURALF$cm/100)^2  # Create BMI 
p <- ggplot(data = EPIDURALF, aes(x = ease, y = BMI, fill = ease)) + 
  guides(fill = FALSE) + scale_fill_grey() 
p1 <- p + geom_violin(scale = "area") + 
  labs(title = "Area", x="", y = "Body Mass Index (BMI)") 
p1    # Left area violin plots
p2 <- p + geom_violin(scale = "count") + 
  labs(title = "Count", x="", y = "Body Mass Index (BMI)")
p2    # Right count violin plots
theme_set(previous_theme)  # Restore original theme


############################## R Code  2.68   ########################### 

previous_theme <- theme_set(theme_bw())   # set black-and-white theme
p <- ggplot(data = EPIDURALF, aes(x = ease, y = BMI))  # Empty plot 
p1 <- p + geom_violin(scale = "count") + 
  geom_boxplot(aes(fill = ease), width = 0.25, outlier.size = 1.25) + 
  scale_fill_grey() + 
  guides(fill = FALSE) + 
  labs(x="", y = "Body Mass Index (BMI)", title = "Count")
p1    # Left violin plots/boxplots
p2 <- p1  + geom_jitter(aes(color = ease), size = 1.25) + 
  scale_color_grey(start = 0.8, end = 0.2) + 
  guides(color = FALSE)
p2    # Right violin plots/boxplots
theme_set(previous_theme)                 # Restore original theme


############################## R Code  2.69   ########################### 

previous_theme <- theme_set(theme_bw())   # set black-and-white theme
p <- ggplot(data = BABERUTH, aes(x = hr, fill = team)) 
p1 <- p + geom_dotplot() + 
  facet_grid(team ~ .) + 
  scale_fill_grey()
p1                                        # left dotplots
p <- ggplot(data = BABERUTH, aes(x = year, y = hr, color = team)) 
p2 <- p + geom_point(size = 2.5) + 
  scale_color_grey() + 
  facet_grid(team ~ .) 
p2                                        # right scatterplots
theme_set(previous_theme)                 # restore original theme


############################## 2.9.5.3 Adding a Smoothed Line   ########################### 

function (mapping = NULL, data = NULL, geom = "smooth",
          position = "identity", method = "auto", formula = y ~ x,
          se = TRUE, n = 80, fullrange = FALSE, level = 0.95,
          na.rm = FALSE, ...)
  
  ############################# R Code 2.70   ########################### 

previous_theme <- theme_set(theme_bw())     # set black-and-white theme
EPIDURALF$BMI <- EPIDURALF$kg/(EPIDURALF$cm/100)^2    # create BMI 
p <- ggplot(data = EPIDURALF, aes(x = cm, y = BMI, color = ease, 
                                  shape = ease)) + 
  labs(x = "Height (cm)", y = "Body Mass Index")
p1 <- p + geom_point() + 
  stat_smooth(method = "loess") + 
  scale_color_grey()
p1        # left scatterplot with loess lines and confidence intervals
p2 <- p + geom_point() + 
  stat_smooth(method = "lm") + 
  scale_color_grey()
p2         # right scatterplot with ols lines and confidence intervals
theme_set(previous_theme)                     # restore original theme


############################# R Code 2.71   ########################### 

previous_theme <- theme_set(theme_bw())  # set black-and-white theme
p <- ggplot(data = EPIDURALF, aes(x = cm, y = BMI, color = ease, 
                                  shape = ease)) + 
  labs(x = "Height (cm)", y = "Body Mass Index")
p1 <- p + geom_point() + 
  stat_smooth(method = "loess", se = FALSE) + 
  scale_color_grey()
p1                                  # Left scatterplot with loess lines
p2 <- p + geom_point() + stat_smooth(method = "lm", se = FALSE, 
                                     fullrange = TRUE) + scale_color_grey()
p2                                  # Right scatterplot with ols lines
theme_set(previous_theme)           # Restore original theme


############################# R Code 2.72   ########################### 

previous_theme <- theme_set(theme_bw())  # set black-and-white theme
library(MASS)
p <- ggplot(data = Animals, aes(x = body, y = brain)) # Empty plot
p1 <- p + geom_point() 
p1  # Top left plot
p2 <- p1 + scale_x_log10() + scale_y_log10() 
p2  # Top center plot log10 axes
p3 <- p2 + labs(x = "Body weight (kg)", y = "Brain weight (g)")
p3  # Top right plot with labeled axes
mod1 <- lm(log10(brain) ~ log10(body), data = Animals)
mod2 <- lm(log10(brain) ~ log10(body), data = subset(Animals, 
                                                     subset = body < 9400))
p4 <- p3 + geom_abline(intercept = coef(mod1)[1], slope = coef(mod1)[2], 
                       linetype = "dashed") 
p4  # Bottom left plot with ols line
p5 <- p4 + geom_abline(intercept = coef(mod2)[1], slope = coef(mod2)[2]) 
p5  # Bottom center plot with ols line (no dinosaurs)
p6 <- p5 + geom_text(data = NULL, x = 0.8, y = 2.6, angle = 54, size = 4, 
                     label = "Solid line omits dinosaurs",) + 
  geom_text(data = NULL, x = 2.6, y = 1.9, angle = 41, size = 4, 
            label = "Dashed line includes dinosaurs")
p6  # Bottom right plot with ols lines labeled with text
theme_set(previous_theme)  # Restore original theme


############################# R Code 2.73   ########################### 

previous_theme <- theme_set(theme_bw())  # set black-and-white theme
Animals$DINO <- ifelse(Animals$body < 9400, "Not Dinosaur", "Dinosaur")
p1 <- ggplot(data = Animals, aes(x = body, y = brain)) + 
  geom_point(size = 3) + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Body weight (kg)", y = "Brain weight (g)") + 
  geom_smooth(data = subset(Animals, subset = DINO == "Not Dinosaur"), 
              method = "lm", se = FALSE, color = "black") + 
  geom_smooth(data = Animals, method = "lm", se = FALSE, 
              linetype = "dashed", color = "black")
p1  # Left plot with ols lines 
p2 <- p1 + geom_text(data = NULL, x = 1.2, y = 3, angle = 52, size = 4, 
                     label = "Solid line omits dinosaurs") + 
  geom_text(data = NULL, x = 3.1, y = 2.3, angle = 39, size = 4, 
            label = "Dashed line includes dinosaurs")
p2  # Right plot with ols lines labeled
theme_set(previous_theme)  # Restore original theme

############################# R Code 2.74   ########################### 

library(maps)       # package has maps
library(mapproj)    # used for different projections
STATESmap <- map_data(map = "state")
p <- ggplot(data = STATESmap, aes(x = long, y = lat, 
                                  group = group, fill = region)) 
p +  geom_polygon(color = "black", alpha = 0.5) + 
  theme_bw() +
  coord_map("polyconic") + 
  theme(legend.position = "none") +
  scale_fill_grey() 

############################# R Code 2.75   ########################### 

library(maps)         # package has maps
library(mapproj)      # used for different projections 
NCmap <- map_data(map = "county", region = "north carolina")
MERGED <- merge(x = NCmap, y = NC2010DMG, by.x = "subregion", 
                by.y = "countyName")
p <- ggplot(data = MERGED, aes(x = long, y = lat, group = group, 
                               fill = evanrate)) +
  labs(fill = "Evangelical\nAdherence\nRate")
p +  geom_polygon(color = "black") + 
  theme_bw() +
  coord_map("polyconic") + 
  scale_fill_gradient2() +
  theme(text = element_text(family = "CM Roman"))


############################# R Code 2.76   ########################### 

mat44 <- matrix(c(1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2, 3, 3, 3, 0), 
                byrow = TRUE, nrow = 4)
mat44
p1 <- ggplot(data = HSWRESTLER, aes(x = tanfat, y = hwfat)) + 
  geom_point(color = "red") + theme_bw()
p2 <- ggplot(data = HSWRESTLER, aes(x = 1, y = tanfat)) + 
  geom_boxplot(fill = "purple") + coord_flip() + labs(x = "") + 
  theme_bw()
p3 <- ggplot(data = HSWRESTLER, aes(x = 1, y = hwfat)) + 
  geom_boxplot(fill = "blue") + 
  labs(x = "") + theme_bw()
multiplot(p1, p3, p2, layout = mat44)

