################### Chapter 05 Script
################### 7/24/15 V.1
library(PASWR2)
################################ Chapter 5 ##############################


################ Figure 5.1 #################################

plot(2, 2, ylim = c(0, 4), xlim =c(0, 4), axes = FALSE, ann = FALSE, type = "n")
f1 <- function(x){(3 - x)}
xs <- seq(0, 3, length = 500)
ys <- f1(xs)
xsx <- c(0,xs, 3)
ysy <- c(0, ys, 0)
polygon(xsx, ysy, col = "lightyellow")
segments(0,0,0,3.8, lwd = 2)
segments(0,0,3.8,0, lwd = 2)
f2 <- function(x){(2 - x)}
xs <- seq(0, 2, length = 500)
ys <- f2(xs)
xsx <- c(0,xs, 2)
ysy <- c(0, ys, 0)
polygon(xsx, ysy, col = "darkgreen")
segments(0,2,2,0, lwd = 2)
segments(0,3,3,0, lwd = 2)
text(0, 4, "$y$")
text(4, 0, "$x$")
arrows(2,3,1,2, lwd = 2, length = 0.1)
arrows(3,2,1.5,0.5, lwd = 2, length = 0.1)
text(2, 3.2, "$y = 3 - x$")
text(3, 2.2, "$y = 2 - x$")
axis(side = 1)
axis(side = 2, las = 1)


################ Example 5.6 #################################

pnorm(50, 50, 1.8974)
pnorm(75, 50, 1.8974)
pnorm(100, 50, 1.8974)
 

################ R Code 5.1 #################################

################ Figure 5.2 #################################

x <- seq(0, 1, length.out = 25)
y <- x
f2 <- function(x, y){ifelse(x >= y, 8*x*y, 0)}
persp(x, y, outer(x, y, f2), shade = 0.6, expand = 0.6,
     theta = 300, phi = 30, ticktype = "detailed", zlab = "z")

################ R Code 5.2 #################################

u <- 1:3
v <- 1:3
M1 <- outer(X = u, Y = v, FUN = "*")
M2 <- u %*% t(v)
M1 == M2
M1

################ R Code 5.3 #################################

x <- c("x1", "x2", "x3")
y <- c("y1", "y2", "y3")
outer(x, y, paste)
t(outer(x, y, paste))

################ Figure 5.3 #################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow= c(1, 3), mar = c(4.1,4.1,5.1,1.1))
x1 <- c(58, 72, 72, 86, 86, 100, 100, 114, 114, 128)
y1 <- c(80, 80, 90, 90, 100, 100, 110, 110, 120, 120)
plot(x1, y1, las = 0, xlab = "$X_1$", ylab = "$Y_1$", main = "$Cov[X_1, Y_1] = 280$", pch = 19)
abline(h = mean(y1), lty = "dashed")
abline(v = mean(x1), lty = "dashed")
x2 <- c(58, 72, 72, 86, 86, 100, 100, 114, 114, 128)
y2 <- c(1200, 1200, 1100, 1100, 1000, 1000, 900, 900, 800, 800)
plot(x2, y2, las = 0, xlab = "$X_2$", ylab = "$Y_2$", main = "$Cov[X_2, Y_2] = -2800$", pch = 19)
abline(h = mean(y2), lty = "dashed")
abline(v = mean(x2), lty = "dashed")
x3 <- c(25.5, 27.0, 30, 33, 34.5, 33, 30, 27, 28.8, 31.2)
y3 <- c(30, 33, 34.5, 33, 30, 27, 25.5, 27, 30, 30)
plot(x3, y3, las = 0, xlab = "$X_3$", ylab = "$Y_3$", main = "$Cov[X_3, Y_3] = 0$", pch = 19)
abline(h = mean(y3), lty = "dashed")
abline(v = mean(x3), lty = "dashed")
par(opar)  # reset settings

################ Example 5.12 #################################

X1 <- c(58, 72, 72, 86, 86, 100, 100, 114, 114, 128)
Y1 <- c(80, 80, 90, 90, 100, 100, 110, 110, 120, 120)
covar <- function(x, y, f){sum((x - mean(x))*(y - mean(y))*f)}
covar(X1, Y1, 1/10)


################ R Code 5.4 #################################

dmultinom(x = c(4, 4, 2), size = 10, prob = c(0.5, 0.3, 0.2))

################ R Code 5.5 #################################

f1 <- function(x, y, p = 0){
 exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f2 <- function(x, y, p = 0.4){
 exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f3 <- function(x, y, p = 0.8){
 exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }

################ R Code 5.6 #################################

################ Figure 5.4 #################################


opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(1.1, 1.1, 1.1, 1.1), pty = "s")
x <- seq(-3, 3, length = 40)
y <- x
persp(x, y, outer(x, y, f1), zlab = "z", main = expression(rho == 0),
     theta = -25, expand = 0.65, phi = 25, shade = 0.4)
persp(x, y, outer(x, y, f2), zlab = "z", main = expression(rho == 0.4),
     theta = -25, expand = 0.65, phi = 25, shade = 0.4)
persp(x, y, outer(x, y, f3), zlab = "z", main = expression(rho == 0.8),
     theta = -25, expand = 0.65, phi = 25, shade = 0.4)
par(opar)
 

################ R Code 5.7 #################################

################ Figure 5.5 #################################


opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 4.1, 1.1), pty = "s")
x <- seq(-3, 3, length = 50)
y <- x
contour(x, y, outer(x, y, f1), nlevels = 10, xlab = "x", ylab = "y",
       main = expression(rho == 0))
contour(x, y, outer(x, y, f2), nlevels = 10, xlab = "x", ylab = "y",
       main = expression(rho == 0.4))
contour(x, y, outer(x, y, f3), nlevels = 10, xlab = "x", ylab = "y",
       main = expression(rho == 0.8))
par(opar)
 

################ R Code 5.8 #################################

################ Figure 5.6 #################################

opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 4.1, 1.1), pty = "s")
x <- seq(-3, 3, length = 50)
y <- x
image(x, y, outer(x, y, f1), col  = gray((0:100)/100), xlab = "x",
     ylab = "y", main = expression(rho == 0))
image(x, y, outer(x, y, f2), col  = gray((0:100)/100), xlab = "x",
     ylab = "y", main = expression(rho == 0.4))
image(x, y, outer(x, y, f3), col  = gray((0:100)/100), xlab = "x",
     ylab = "y", main = expression(rho == 0.8))
par(opar)
 

################ R Code 5.9 #################################

x <- seq(-3, 3, length = 40)
y <- x
z1 <- outer(x, y, f1)
z2 <- outer(x, y, f2)
z3 <- outer(x, y, f3)
Grid <- expand.grid(x = x, y = y)
zp <- c(expression(rho == 0.0), expression(rho == 0.4),
       expression(rho == 0.8))

################ R Code 5.10 #################################

################ Figure 5.7  #################################

wireframe( z1 + z2 + z3 ~ x * y, data = Grid, xlab = "x", ylab = "y",
         zlab = "z", outer = TRUE, layout = c(3, 1),
         strip = strip.custom(factor.levels = zp) )


################ R Code 5.11 #################################

################ Figure 5.8  #################################
 
contourplot(z1 + z2 + z3 ~ x * y, data = Grid, xlab = "x", ylab = "y",
             outer = TRUE, layout = c(3, 1), aspect = "xy",
             cuts = 11, strip = strip.custom(factor.levels = zp))
  

################ R Code 5.12 #################################

################ Figure 5.9  #################################

levelplot(z1 + z2 + z3 ~ x * y, data = Grid, xlab = "$x$", ylab = "$y$", 
          outer = TRUE, layout = c(3, 1), aspect = "xy",
          strip = strip.custom(factor.levels = zp))

################ R Code 5.13 #################################

x <- seq(-3, 3, length = 50)
y <- x
z1 <- outer(x, y, f1)
z2 <- outer(x, y, f2)
z3 <- outer(x, y, f3)
Grid <- expand.grid(x = x, y = y)
DF1 <- data.frame(x = Grid$x, y = Grid$y, z = as.vector(z1))
DF2 <- data.frame(x = Grid$x, y = Grid$y, z = as.vector(z2))
DF3 <- data.frame(x = Grid$x, y = Grid$y, z = as.vector(z3))
DF1$r = "rho == 0.0"
DF2$r = "rho == 0.4"
DF3$r = "rho == 0.8"
BDF <- rbind(DF1, DF2, DF3)
 

################ R Code 5.14 #################################

################ Figure 5.10  #################################

p <- ggplot(data = BDF, aes(x = x, y = y, z = z))
p + stat_contour(aes(colour = ..level..)) + theme_bw() +
 scale_colour_gradient(low = "gray10", high = "gray90") +
 labs(colour = "Density", x = "x", y = "y") +
 facet_grid(. ~ r, labeller = label_parsed) +
 coord_fixed(ratio = 1)

################ R Code 5.15 #################################

################ Figure 5.11  #################################

p <- ggplot(data = BDF, aes(x = x, y = y, fill = z))
p + geom_raster() + theme_bw() +
 scale_fill_gradient(low = "gray10", high = "gray90") +
 labs(fill = "Density", x = "x", y = "y") + facet_grid(. ~ r) +
 coord_fixed(ratio = 1)
 

################ Example 5.19 ################################# 

pnorm(1.8, 2.4, 0.6)
 
pnorm(1.8, 1.77, 0.48)
 
1 - pnorm(3, 2.4, 0.6)
 
1 - pnorm(3, 1.77, 0.48)

