################### Chapter 01 Script
################### 7/23/15 V.1
library(PASWR2)
################### R code 1.1

options(width = 70)           # set width of console output
set.seed(12)                  # set seed to make results reproducible
ruv <- runif(n = 20, min = 0, max = 1)
round(ruv, 4)                 # round answers to 4 decim 

(7 * 3) + 12/2 - 7^2 + sqrt(4)

################### R code 1.2

x <- 5              # vector of length one
y <- c(7, 3, 5)     # combining the values into y
z <- c(2, 4, 6, 8)  # combining the values into z
x + y               # adding x and y
c(5, 5, 5) + y      # the shorter vector is recycled 3 times
y + z               # adding y and z
c(y, 7) + z         # values of y recycled to length of z


################### R code 1.3 

LogVec <- (x < z)  # logical vector
LogVec             # 5 < 2, 5 < 4, 5 < 6, 5 < 8
typeof(LogVec)     # determine how LogVec is stored internally


################### R code 1.4

X <- c(FALSE, TRUE, FALSE)
Y <- c(FALSE, TRUE, TRUE)
X & Y   # Boolean X intersection Y
X | Y   # Boolean X union Y
X == Y  # Boolean EQUALITY
X != Y  # Boolean INEQUALITY

X && Y  # only looks at first element of each vector (intersection)
X || Y  # only looks at first element of each vector (union)


################### R code 1.5

Y[3] && X[2]  # compares 3 element of Y to 2 element of X
X[2] && Y[2]  # compares second element of each vector (intersection)
X[2] && Y[3]  # compares element 2 of X and element 3 of Y (intersection)
X[3] || Y[3]  # compares third element of both vectors (union)


################### R code 1.6

typeof(z)               # determine how z is stored internally
z1 <- as.integer(z)     # coerce z from double to integer
z1
typeof(z1)              # determine how z is stored internally
IntVec <- c(LogVec, z1) # integer  vector
IntVec
typeof(IntVec)          # determine how IntVec is stored internally

NumVec <- c(LogVec, z)  # numeric vector
NumVec
typeof(NumVec)  # determine how NumVec is stored internally

ComVec <- c(NumVec, 0i)  # complex vector
ComVec
typeof(ComVec)  # determine how ComVec is stored internally

ChrVec <- c(ComVec, "dog")   # a character vector
ChrVec
typeof(ChrVec)   # determine how ChrVec is stored internally

Lst <- c(ChrVec, list(x = 4)) # a list
typeof(Lst)  # determine how Lst is stored internally

################### R code 1.7

NV <- c(-4, 0, 2, 4, 6)  # numeric vector
NV/NV                # 0/0 is not a number (NaN)
sqrt(NV)  # cannot take square root of -4
NV^9999  # very large and small numbers use -Inf and Inf

NV[-1]               # omit first element of NV
NV[c(1, 3)]          # extract first and third element of NV
CV <- LETTERS[c(1, 2, 3, 4)]  # first four upper case letters of CV
CV
CV[c(2, 3)]          # Extract second and third elements of CV
LV <- c(TRUE, FALSE, TRUE, TRUE)
LV
LV[-2]               # omit second element of LV
LV[-c(1, 3)]         # omit first and third elements of LV

################### R code 1.8

24:20            # values 24, 23, 22, 21, and 20
letters[24:20]   # 24th through 20th lowercase letters
seq(from = 5, to = 25, by = 5)
seq(from = 5, by = 5, length.out = 5)
seq(from = 23, to = 22, length.out = 5)

################### R code 1.9

rep(x = 5, times = 10)
rep(x = c(TRUE, FALSE), times = 10)
rep(x = letters[5:8], each = 2)
rep(x = 13:11, times = 1:3)
rep(x = 1:5, length.out = 20)

################### R code 1.10

FEV <- rep(x = 3:1, times = 1:3)
FEV
FEV * FEV
FEV * FEV > 3
FEV[FEV * FEV > 3]
# An equivalent approach using subset
subset(x = FEV, subset = FEV * FEV > 3)
# Instead of the actual values we may want the indices
# where these values occur
which(FEV * FEV > 3)

################### R code 1.11

Num <- c(1, pi, 5)
Log <- c(TRUE, FALSE, TRUE)
Chr <- c("a", "character", "vector")
mode(Num)
class(Num)
mode(Log)
class(Log)
mode(Chr)
class(Chr)

################### R code 1.12

x2 <- 1:5
Y2 <- x2 + rnorm(n = 5, mean = 0, sd = .5)  # add normal errors
model <- lm(Y2 ~ x2) # Regressing Y2 onto x2
mode(model)          # model has mode list
class(model)         # model has class lm

################### R code 1.13

example(median)

args(lm)


################### R code 1.14

A1 <- array(data = 1:24, dim = c(3, 4, 2))
A1
is.array(A1)
is.matrix(A1)
class(A1)
dim(A1)

M1 <- array(data = 1:24, dim = c(4, 6))
M1
is.array(M1)
is.matrix(M1)
class(M1)
dim(M1)

M2 <- matrix(data = 1:24, nrow = 4, byrow = TRUE)   # row-major order
M2
M3 <- matrix(data = 1:24, ncol = 6, byrow = FALSE)  # column-major order
M3

Data <- c(190, 8, 22.0, 191, 4, 1.7, 223, 80, 2.0)
barley.data <- matrix(data = Data, nrow = 3, byrow = TRUE) 
barley.data

dim(barley.data)

province <- c("Navarra", "Zaragoza", "Madrid")
type <- c("typeA", "typeB", "typeC")

dimnames(barley.data) <- list(province, NULL)
barley.data

dimnames(barley.data) <- list(NULL, type)
barley.data

dimnames(barley.data) <- list(province, type)
barley.data

dimnames(barley.data)

dimnames(barley.data) <- NULL
barley.data

dimnames(barley.data)  <- list(province, type)
barley.data[2, ]

barley.data["Zaragoza", ]

barley.data[, "typeC"]

typeD <- c(2, 3.5, 2.75)
barley.data <- cbind(barley.data, typeD)
rm("typeD")  # remove typeD from workspace
barley.data

apply(X = barley.data, MARGIN = 2, FUN = mean)

apply(X = barley.data, MARGIN = 1, FUN = mean)

x <- c(1, 2, 3)
names(x) <- c("A", "B", "C")
x

names(x) <- NULL
x

################### R code 1.15

barley.data[2, ]
dim(barley.data[2, ])  # Not a matrix...a vector now
is.vector(barley.data[2, ])
barley.data[2, ,drop = FALSE]  # A 1*4 matrix not a vector
is.matrix(barley.data[2, , drop = FALSE])
dim(barley.data[2, , drop = FALSE])

as.matrix(barley.data[2, , drop =FALSE])  # 1*4 matrix
t(as.matrix(barley.data[2, , drop = FALSE]))  # 4*1 matrix

A <- matrix(c(3, 2, 1, 2, -3, 1, 1, 1, 1), byrow = TRUE, nrow = 3)
A
b <- matrix(c(10, -1, 6), byrow = TRUE, nrow = 3)
b
x <- solve(A, b)
x

A %*% x

################### R code 1.16

v <- c(2, 1, 0, 2)
fv <- factor(v)
fv
fv <- factor(v, levels = 0:2, labels = c("Easy", "Difficult", "Impossible"))
fv


################### R code 1.17

v2 <- c("Impossible", "Difficult", "Easy", "Impossible")
fv2 <- factor(v2)
fv2


################### R code 1.18

fv3 <- factor(v2, levels = c("Easy", "Difficult", "Impossible"))
fv3
levels(fv2) <- c("Easy", "Difficult", "Impossible")
fv2


################### R code 1.19

stu1 <- list(first.name = "Bob", last.name = "Smith",
             major = "Statistics", semester.hours = 18, 
             grades = c("A", "B+", "A-", "C+", "B", "B-"))
names(stu1$grades) <- c("Analysis", "Experimental Design", "English", 
                        "German", "Regression", "Programming")
stu1
names(stu1)

################### R code 1.20

stu2 <- list("Bob", "Smith", "Statistics", 18,
             c("A", "B+", "A-", "C+", "B", "B-"))
stu2
names(stu2)  # tags not used


################### R code 1.21

stu1$major
stu1[["major"]]
stu1[[3]]


################### R code 1.22

stu1$grades["Programming"]
stu1[["grades"]]["Programming"]
stu1[[5]]["Programming"]
stu1[[5]][6]


################### R code 1.23

nv <- c(1, 3, 6, 8)                   # Numeric vector
cv <- c("a", "d", "f", "p")           # Character vector
lv <- c(TRUE, FALSE, FALSE, TRUE)     # Logical vector
DF1 <- data.frame(nv, cv, lv)
DF1
str(DF1)
DF2 <- data.frame(nv, cv, lv, row.names = c("Joe", "Bob", "Jill", "Sam"), 
                  stringsAsFactors = FALSE)

DF2
str(DF2)
rm("nv", "cv", "lv")  # remove the variables from the current environment

row.names(DF1) <- c("Joe", "Bob", "Jill", "Sam")
DF1

################### R code 1.24

DF2
nv            # nv not on search path it is part of DF2
DF2$nv        # dollar prefixing
DF2[[1]]      # component indexing
DF2[["nv"]]   # component naming
DF2[, "nv"]   # all rows, column nv
DF2[, 1]      # all rows, column 1
with(data = DF2, expr = nv)
attach(DF2)   # DF2 on search path
nv
detach(DF2)   # DF2 removed from search path
nv            # nv no longer on search path

search()     # show attached packages

ls(1)        # shows objects in .GlobalEnv 
attach(DF2)  # place DF2 in search path pos. 2
search()     # shows DF2 in pos. 2
ls(2)        # shows objects in pos. 2 (DF2)

nv <- nv + 5  # nv stored in workspace
nv            # nv from pos. 1
ls(2)         # list objects in position 2
DF2$nv        # nv from pos. 2
detach(DF2)   # remove DF2 from search path
search()      # show attached packages

DF2$nv <- DF2$nv + 5  # nv changed inside DF2
DF2


################### R code 1.25

DF2[c(1, 3)]  # extract 1st and 3rd columns
DF2[c("nv", "lv")]  # extract columns named nv and lv
DF2[ , c(1, 3), drop = FALSE]  # extract all rows for 1st and 3rd columns
DF2[ , c("nv", "lv"), drop = FALSE] # all rows for columns nv and lv

subset(x = DF2, select =c(nv, lv))


################### R code 1.26

library(MASS)  # Places MASS in search path
help(Animals)  # Opens HTML help window
head(Animals)  # shows first six rows


site <- "http://www1.appstate.edu/~arnholta/PASWR/CD/data/Bodyfat.txt"
FAT <- read.table(file = site, header = TRUE, sep = "\t")
head(FAT)     # Show first six rows

site <- "http://www1.appstate.edu/~arnholta/PASWR/CD/data/Bodyfat.csv"
FAT <- read.csv(file = site)
head(FAT)     # Show first six rows



################### R code 1.27

site <- paste0("http://data.baltimorecity.gov/api/",
               "views/7ymi-bvp3/rows.csv?accessType=DOWNLOAD")
download.file(site, destfile = "Salaries.csv")
list.files("./")                     # show files in ./Data
file.info("Salaries.csv")            # file information
BES <- read.csv("Salaries.csv")   
head(BES, n = 2)                         # show first two rows  

site <- "http://bit.ly/12H9E0l"  # URL
download.file(site, destfile = "Salaries.csv.zip")
file.info("Salaries.csv.zip")['size']


################### R code 1.28

library(repmis)
URL <- "https://db.tt/1rlTfYnk"
Verizon1 <- source_data(url = URL, sep = ",", header = TRUE)
head(Verizon1)  # show first six rows of data



################### R code 1.29

URL <- "http://bit.ly/1gqZCX3"
Verizon2 <- source_data(url= URL)
head(Verizon2) # show first six rows of data



################### R code 1.30

Verizon3 <- source_DropboxData("Verizon.csv", "a9muo5wybukfs86")
head(Verizon3)

age1 <- scan()
age1
age2 <-scan()
age2



################### R code 1.31

library(xlsx)  # Loading xlsx package
Faculty1 <- read.xlsx(file = "../Data/FACULTY.xlsx", sheetName = "Univ1")
Faculty1
Faculty2 <- read.xlsx(file = "../Data/FACULTY.xlsx", sheetName = "Univ2")
Faculty2


################### R code 1.32

URL <- "http://bit.ly/1iOWsGP"
Faculty3 <- repmis::source_XlsxData(url = URL, sheet = "Univ1")
Faculty4 <- repmis::source_XlsxData(url = URL, sheet = "Univ2")

Faculty3
Faculty4

write.table(FAT, file = "FAT.txt")

write.table(FAT, file="FAT.txt", sep = "\t")

write.xlsx(FAT, file = "FAT.xlsx")

site <- "http://www1.appstate.edu/~arnholta/PASWR/CD/data/Poplar3.CSV"
poplar <- read.csv(file = url(site))
head(poplar, n = 3)  # show first three rows

str(poplar)
summary(poplar)

poplarC <- read.csv(file = url(site), na.strings = "-99", 
                    colClasses = c(rep("factor", 3), rep("numeric", 3), "factor"))
str(poplarC)

DF <- read.table(file=url(site), header=TRUE)
df <- DF
df[df$var1==999,  "var1"] = NA
df[df$var2==99,   "var2"] = NA
df[df$var3==9999, "var3"] = NA

levels(poplarC$Site) <- c("Moist", "Dry")
TreatmentLevels <- c("Control", "Fertilizer", "Irrigation", "FertIrriga")
levels(poplarC$Treatment) <- TreatmentLevels
str(poplarC$Treatment)


################### R code 1.33
site <- "http://www1.appstate.edu/~arnholta/PASWR/CD/data/Poplar3.CSV"
poplar <- read.csv(file = url(site))
head(poplar, n = 3) # show first three rows


################### R code 1.34

poplarC$Site <- factor(poplarC$Site, labels = c("Moist", "Dry"))
str(poplarC$Site)



################### R code 1.35

poplar$Site <- factor(poplar$Site, levels = c("Moist", "Dry"))
str(poplar$Site)



################### R code 1.36

library(PASWR2)
levels(EPIDURALF$ease)  # Default levels (alphabetical)
EPIDURALF$ease <- factor(EPIDURALF$ease, levels = c("Easy", "Difficult", "Impossible"))
levels(EPIDURALF$ease)  # Correct levels
rm(EPIDURALF)

summary(poplarC[, 4:7])  # summary of columns 4-7



################### R code 1.37

dim(poplarC)
myNoMissing <- na.omit(poplarC)
summary(myNoMissing[, 4:7])  # summary of columns 4-7
dim(myNoMissing)



################### R code 1.38

poplarC[c(178:180, 209:211, 217:219), ]
myNoMissing[c(178:179, 208:209, 215:216), ]


################### R code 1.39

complete <- complete.cases(poplarC)
myCompleteCases <- poplarC[complete, ]
dim(myCompleteCases)
summary(myCompleteCases[, 4:7])  # summary of columns 4-7 
myCompleteCases[c(178:179, 208:209, 215:216), ]

x <- c(1, 6, 9, 2, NA)
is.na(x)
!is.na(x)
x[!is.na(x)]


################### R code 1.40

attach(EPIDURALF)
BMI = kg/(cm/100)^2               # Creating new variable
detach(EPIDURALF)
EPIbmi2 <- cbind(EPIDURALF, BMI)  # Column binding BMI to df
rm(BMI)                           # removing BMI from .GlobalEnv
EPIbmi2[1:3, -5]   # Show first 3 rows of EPIbmi2 w/o treatment 



################### R code 1.41

levels(EPIbmi2$ease)
EPIbmi2$ease <- factor(EPIbmi2$ease, levels = c("Easy", "Difficult", "Impossible"))
levels(EPIbmi2$ease)

EPIDURALF$BMI <- EPIDURALF$kg/(EPIDURALF$cm/100)^2
rm(EPIDURALF)

levels(EPIDURALF$ease)
EPIbmi <- within(data = EPIDURALF, expr = {
  BMI = kg/(cm/100)^2
  ease = factor(ease, levels = c("Easy", "Difficult", "Impossible"))
})
EPIbmi[1:6, -5]   # Show first 6 rows of EPIbmi w/o treatment
levels(EPIbmi$ease)


################### R code 1.42

x <- c(1, 1, 1, 3, 3, 3, 2, 2, 2)
y <- c(3, 2, 3, 6, 2, 6, 10, 4, 4)
z <- c(7, 4, 2, 9, 6, 4, 5, 3, 1)
DF <- data.frame(x, y, z)
rm(x, y, z)  # remove x, y, and z from workspace
t(DF)  # transpose DF
with(data = DF, t(DF[order(x,y,z), ]))

subEPI <- EPIbmi[1:6, c("oc", "ease", "BMI")]
subEPI

myO <- order(subEPI$oc, subEPI$ease, subEPI$BMI)
myO
subEPI[myO, ]

DFphy <- data.frame(ID = 1:6, Gender = rep(c("Female", "Male"), 
                                           each = 3), HDL = c(39, 42, 22, 27, 29, 45))
DFphy
DFsci <- data.frame(ID = c(2, 4, 3, 5, 1, 6), 
                    secretID = rep(c("Drug", "Placebo"), each = 3))
DFsci
merge(DFphy, DFsci)


################### R code 1.43

head(BODYFAT, n = 3)  # show first 3 rows of BODYFAT
BODYFAT$fat < 25
with(data = BODYFAT, fat < 25)
attach(BODYFAT)
fat < 25
detach(BODYFAT)



################### R code 1.44

BODYFAT$fat[BODYFAT$fat < 25]
with(data = BODYFAT, fat[fat < 25])
attach(BODYFAT)
fat[fat < 25]
detach(BODYFAT)
BODYFAT[BODYFAT$fat < 25, "fat"]
BODYFAT[BODYFAT$fat < 25, 2]



################### R code 1.45

BODYFAT[BODYFAT$fat < 25, c(2, 3)]  # fat < 25 for columns 2 and 3
BODYFAT[BODYFAT$fat < 25, c("fat", "sex")] # using names of columns 

subset(BODYFAT, select = fat, subset = fat < 25, drop = TRUE)



subset(x = BODYFAT, select = c(fat, sex), subset = fat < 25)

BODYFAT[BODYFAT$fat < 25, ]  # fat < 25 all columns
with(data = BODYFAT, BODYFAT[fat < 25, ])  # fat < 25 all columns
attach(BODYFAT)
BODYFAT[fat < 25, ]  # fat < 25 all columns
detach(BODYFAT)

subset(x = BODYFAT, subset = fat < 25)

with(BODYFAT, fat[fat < 25 & fat != 7.8])

with(BODYFAT, BODYFAT[fat < 25 & fat != 7.8, ])

subset(x = BODYFAT, subset = fat < 25 & fat != 7.8)


################### R code 1.46

any(BODYFAT$fat < 10 & BODYFAT$sex == "M")    # Condition TRUE for any?
all(BODYFAT$fat < 10 & BODYFAT$sex == "M")    # Condition TRUE for all?
which(BODYFAT$fat < 10 & BODYFAT$sex == "M")  # Indices for which TRUE

table(EPIbmi2$ease)

table(EPIbmi2$ease, EPIbmi2$doctor)

with(data = EPIbmi2, table(ease, doctor))
attach(EPIbmi2)
table(ease, doctor)
detach(EPIbmi2)

xtabs(formula = ~ ease + doctor, data = EPIbmi2)

table(EPIbmi2$ease, EPIbmi2$doctor, EPIbmi2$treatment)
xtabs(~ ease + doctor + treatment, data = EPIbmi2)

ftable(EPIbmi2$treatment, EPIbmi2$ease, EPIbmi2$doctor)

ftable(doctor ~ treatment + ease, data = EPIbmi2)



################### R code 1.47

CT <- table(EPIbmi2$ease, EPIbmi2$doctor)
CT
margin.table(CT)     # sum all entries in table
margin.table(CT, 1)  # sum entries across rows
margin.table(CT, 2)  # sum entries down columns
addmargins(CT)       # show margins



################### R code 1.48

prop.table(CT)       # Equivalent to CT/margin.table(CT)
prop.table(CT, 1)    # divide each entry of CT by its row total
prop.table(CT, 2)    # divide each entry of CT by its column total



################### R code 1.49

fBMI <- cut(EPIbmi$BMI, breaks = 3)  # factor BMI with 3 levels
table(fBMI)
rm(fBMI)



################### R code 1.50

EPIbmi4 <- within(data = EPIbmi, expr = {
  fBMI <- cut(BMI, breaks = 3, include.lowest = TRUE)
  flBMI <- factor(fBMI, labels = c("Low", "Med", "High"))
})
head(EPIbmi4)[, c("BMI", "fBMI", "flBMI")]
levels(EPIbmi4$fBMI)


################### R code 1.51

EasyDocA <- with(data = EPIbmi4,  BMI[ease == "Easy" & doctor == "A"])
DiffDocC <- with(data = EPIbmi4,  BMI[ease == "Difficult" & 
                                        doctor == "C"])
c(mean(EasyDocA),  mean(DiffDocC))


################### R code 1.52

with(data = EPIbmi4,
     tapply(X = BMI,  INDEX = list(ease, doctor), FUN = mean)
)



################### R code 1.53

AgDF <- with(data = EPIbmi4,  
             aggregate(x = BMI, by = list(ease, doctor), FUN = mean)
)
AgDF

x <- c(19, 14, 15, 17, 20, 23, 19, 19, 21, 18)
treatment <- c(rep("A", 5), rep("B", 5))
treatment

treatment <- rep(LETTERS[1:2], rep(5, 2))
treatment

ANSWER <- tapply(x, treatment, mean)
ANSWER            # show results
ANSWER['B']       # show only mean of B 
is.array(ANSWER)  # confirms results are stored in an array
rm(x, treatment)  # cleanup workspace


dpois(x = 0:10, lambda = 3)



################### R code 1.54

sum.a <- 0
for(i in c(10, 20, 30)){
  sum.a <- i + sum.a
}
sum.a



################### R code 1.55

Number <- 15              # Number of Lucas numbers desired 
Lucas <- numeric(Number)  # Storage for Lucas numbers
Lucas[1] <- 2             # First Lucas number 
Lucas[2] <- 1             # Second Lucas number
for(i in 3:Number){
  Lucas[i] <- Lucas[i - 1] + Lucas[i - 2]
}
Lucas


################### R code 1.56

options(digits = 8)
x <- 113734
tolerance <- 0.00001
oldapp <- x/2
newapp <- (oldapp + x/oldapp)/2 
i <- 0
while( abs(newapp - oldapp) > tolerance){
  oldapp <- newapp
  newapp <- (oldapp + x/oldapp)/2
  i <- i + 1 # Iteration number
}
c(newapp, i)
options(digits = 7) # reset to default



################### R code 1.57

options(digits = 8)
x <- 113734
tolerance <- 0.00001
oldapp <- x/2
newapp <- (oldapp + x/oldapp)/2 
i <- 0
repeat{
  oldapp <- newapp
  newapp <- (oldapp + x/oldapp)/2
  i <- i + 1
  if(abs(newapp - oldapp) < tolerance)
    break
}
c(newapp, i)
options(digits = 7) # reset to default


set.seed(3)           # setting seed for reproducibility
N <- 10^5 - 1         # N = number of simulations
means <- numeric(N)   # Defining numeric vector of size N
for(i in 1:N){means[i] <- mean(sample(x = 1:6, size = 2, replace = TRUE))}
T1 <- table(means)
T1

set.seed(3)  # setting seed for reproducibility
i <- 1
N <- 10^5 - 1  # N = number of simulations
N2mat <- matrix(0, N, 2)  # initialize N*2 matrix to all 0's
while(i <= N){N2mat[i, ] <- sample(x = 1:6, size = 2, replace = TRUE)
i <- i + 1
}
means <- apply(N2mat, 1, mean)
T2 <- table(means)
T2

set.seed(3)  # setting seed for reproducibility
i <- 1
N <- 10^5 - 1  # N = number of simulations
N2mat <- matrix(0, N, 2)  # initialize N*2 matrix to all 0's
repeat{N2mat[i, ] <- sample(1:6, 2, replace = TRUE)
if (i == N) break
i <- i + 1
}
means <- apply(N2mat, 1, mean)
T3 <- table(means)
T3

par(yaxt="n")
plot(T3/N, xlab = "Mean of Two Dice", ylab = "", 
     main="Simulated Sampling Distribution \n of the Sample Mean",
     ylim=c(0, 6.1/36), lwd = 2, cex.main = 1)
par(yaxt="s")
axis(side=2, at=c(0, 1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36), 
     labels=c("0","1/36", "2/36", "3/36", "4/36", "5/36", "6/36", "5/36", 
              "4/36", "3/36", "2/36", "1/36"), las=1 )
abline(h=c(0:6)/36, lty=2, col="gray")



################### R code 1.58

MBMI <- with(data = EPIbmi4, tapply(BMI, INDEX = list(ease, doctor), FUN = mean))
MBMI  # show mean BMI values


################### R code 1.59

CBMI <- with(data = EPIbmi4,
             ifelse(test = tapply(BMI, INDEX = list(ease, doctor), FUN = mean) > 35, 
                    yes = "Obese", no = "Not-Obese"))
CBMI 

fname <- function(argument1, argument2, ...){expression}

SUM.N <- function(n){n*(n + 1)/2}

SUM.N(n = 4)

sum.sq <- function(x){sum(x^2)}

sum.sq(SUM.N(n = 4))



################### R code 1.60

args(sample)
set.seed(13)
Named <-  sample(size = 20, replace = TRUE, x = 10)
Named



################### R code 1.61

set.seed(13)
Position <- sample(10, 20, TRUE)
Position


################### R code 1.62

set.seed(13)
Mixture <- sample(10, TRUE, size = 20)
Mixture

source("C:/Rfolder/functions.txt")   # For Windows
source("/Rfolder/functions.txt")     # For Mac/Unix-like

SIMcraps <- function(n.games = 10000){
  opar <- par(no.readonly = TRUE)
  options(scipen=999)  # Suppress scientific notation in the output
  # linebet returns 0 or 1 based on whether 'shooter' loses or wins
  linebet <- function(){
    comeoutroll <- sum(sample(1:6, 2, replace = TRUE))  # first throw
    if (comeoutroll %in% c(7, 11)){
      result <- 1  # win if comeoutroll is 7 or 11
    } else if (comeoutroll %in% c(2, 3, 12)) {
      result <- 0  # loss if comeoutroll is 2, 3, or 12
    } else {
      repeat{
        substhrow <- sum(sample(1:6, 2, replace = TRUE))
        # subsequent throw
        if(substhrow == comeoutroll){
          result <- 1  # win if substhrow same as comeoutroll
          break
        } else if (substhrow == 7){
          result <- 0 # loss if substhrow is a 7
          break
        }
      }
    }
    result
  }
  gameOutcome <- numeric(n.games)  # vector of 0s of length n.games
  # Play n.games simulated crap games
  for(i in 1:n.games){gameOutcome[i] <- linebet()}    
  # gameOutcomes is a vector of wins and losses
  P.win <- mean(gameOutcome)       # Percent of time shooter wins
  Actual.answer <- 244/495
  Error <- round(abs(Actual.answer - P.win)/Actual.answer * 100, 4)
  cat("Simulated probability of winning =",  P.win, "based on",
      n.games, "simulated games.", "\n")
  cat("Percent simulation error based on actual answer 244/495 is ",
      Error,"%.", "\n", sep="")
  par(opar)
  options(scipen = 0)
}

set.seed(1234)  # setting seed for reproducibility
SIMcraps(n.games = 50000)


################### R code 1.63

Industry <- c("A", "B", "C", "D", "A", "A", "B", "B", "D", "D", "D", 
              "A", "C", "C", "C")
Value <- c(1, 4, 7, 9, NA, 2, 5, 6, NA, 10, 11, 3, 8, NA, NA)
DF <- data.frame(Industry, Value)
rm(Industry, Value)  # remove Industry and Value from work space
DF$Ivalue <- ifelse(is.na(DF$Value),
                    ave(DF$Value, DF$Industry, 
                        FUN = function(x){median(x, na.rm = TRUE)}),
                    DF$Value) 

ODF <- DF[order(DF$Industry, DF$Value), ] # Order DF by Industry and Value
ODF
MED <- tapply(ODF$Value, ODF$Industry, median, na.rm = TRUE)
MED

################### R code 1.64

EPIbmi <- within(data = EPIDURALF, expr = {
  BMI = kg/(cm/100)^2
  ease = factor(ease, levels = c("Easy", "Difficult", "Impossible"))
})
with(data = EPIbmi, 
     is.numeric(BMI))
with(data = EPIbmi,
     is.numeric(kg))
with(data = EPIbmi,
     is.factor(ease))
with(data = EPIbmi,
     is.factor(treatment))
is.ts(sunspots)  # is time series sunspots


################### R code 1.65

par(mfrow= c(3, 3))     # graphics device with 3 rows and 3 columns
with(data = EPIbmi,
     plot(BMI ~ kg, main = "Scatterplot"))        # num ~ num
with(data = EPIbmi,
     plot(BMI ~ ease, main = "Boxplots"))         # num ~ factor
with(data = EPIbmi,
     plot(table(ease), main = "Barplot"))         # 1D table
with(data = EPIbmi,
     plot(ease, main = "Barplot"))                # factor
with(data = EPIbmi,
     plot(ease ~ BMI, main = "Spinogram"))        # factor ~ num
with(data = EPIbmi,
     plot(BMI, ease, main = "Stripchart"))        # num, factor
with(data = EPIbmi,
     plot(ease ~ treatment, main = "Spine plot")) # factor ~ factor
with(data = EPIbmi,
     plot(table(ease, treatment), main = "Mosaic plot"))  # 2D table
plot(sunspots, main = "Time-series plot")         # time series
par(mfrow=c(1, 1))     # graphics device with 1 row and 1 column


################### R code 1.66

par(mfrow= c(3, 3))     # graphics device with 3 rows and 3 columns
with(data = EPIbmi,
     plot(BMI ~ kg, main = "Scatterplot"))        # num ~ num
with(data = EPIbmi,
     plot(BMI ~ ease, main = "Boxplots"))         # num ~ factor
with(data = EPIbmi,
     plot(table(ease), main = "Barplot"))         # 1D table
with(data = EPIbmi,
     plot(ease, main = "Barplot"))                # factor
with(data = EPIbmi,
     plot(ease ~ BMI, main = "Spinogram"))        # factor ~ num
with(data = EPIbmi,
     plot(BMI, ease, main = "Stripchart"))        # num, factor
with(data = EPIbmi,
     plot(ease ~ treatment, main = "Spine plot")) # factor ~ factor
with(data = EPIbmi,
     plot(table(ease, treatment), main = "Mosaic plot"))  # 2D table
plot(sunspots, main = "Time-series plot")         # time series
par(mfrow=c(1, 1))     # graphics device with 1 row and 1 column

par(mfrow=c(3, 3), pty = "m")  #3 by 3 layout
x <- -4:4
y <- x^2
plot(x, y, xlim=c(-8, 8), ylim = c(0, 20), main ="")
title(main = "Default values with limits \n for x and y axes altered")
plot(x, y, pch = "x", xlim=c(-8, 8), ylim = c(0, 20), main="")
title(main = "Default plotting character \n changed to x")
plot(x, y, type = "l", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Lines connecting the data")
plot(x, y, type = "b", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Both point and lines \n between data")
plot(x, y, type = "h", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Vertical lines")
plot(x, y, type = "o", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Overlaid points \n and connected lines")
plot(x, y, type = "s", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Stairsteps")
plot(x, y, xlim = c(-8, 8), ylim = c(0, 20), main = "", xlab = "X Axis",
     ylab = "Y Axis")
title(main = "Basic plot with axes labeled")
plot(x, y, type = "n", xlim = c(-8, 8), ylim = c(0, 20), xlab = "",
     ylab = "", main = "")
title(main = "Empty Graph \n(No Plotted Points)")
text(0, 16, "Some Red Text", col = "red")
text(0, 10, expression(paste("Some Math: ", bar(x)==sum(frac(x[i],
                                                             n), i==1, n))))
Alpha <- round(mean(y), 3)
text(0, 3, bquote(paste("The Mean: ", bar(y)==.(Alpha))))
par(mfrow=c(1, 1))



################### R code 1.67

f <- function(x){sin(1+x^2)/(1+x^2) + x^2/100}
plot(f, from = -10, to = 10, n = 1000, xlab = "", ylab = "")
curve(f, from = -10, to = 10, n = 1000, xlab = "", ylab = "")




################### R code 1.68

x <- -4:4; y <- x^2                  # Create two vectors x and y
# Create figure margins of 5.1 lines for all four sides, create outer
# margins of 2.1 lines for all sides, set back ground color to grey90
par(mar = rep(5, 4) + 0.1, oma = rep(2, 4) + 0.1, bg = "grey90")
plot(x, y, type = "n")               # Creates empty plot
box("outer")                         # draws outer box
par(xpd = TRUE)                      # clipping set to figure region
rect(-7, -6, 7, 23, col = "grey95")  # large rectangle (figure region)
box("figure")                        # draws figure box
par(xpd = FALSE)                     # clipping set back to plot region
rect(-7, -6, 7, 23, col = "grey90")  # large rectangle (plot region)
box("plot")                          # draws box around plot region
axis(side = 1)                       # add axis to bottom
axis(side = 2)                       # add axis to left side
# label sides
mtext(text = paste("Side", 1:4), side = 1:4, line = -1.5, cex = 1.5)
# label outer margins
mtext(text = paste("Outer Margin", 1:4), side = 1:4, line = 0.5,
      cex=1.5, outer = TRUE, col="blue")
# label Figure Region
mtext(text = "Figure Region", side = 3, at = 4, line = 2, cex = 2,
      col = "gray40")
# show Lines -1 to 4 at x = -3 for sides 1 and 3 (bottom & top)
for(side in c(1, 3))
  mtext(text = paste("Line", -1:4), side = side, line = -1:4, at = -3,
        col = "red")
# show Lines -1 to 4 at y = 2.5 for sides 2 and 4 (left & right)
for(side in c(2, 4))
  mtext(text = paste("Line", -1:4), side = side, line = -1:4, at = 2.5,
        col = "red")
# show Lines -1 to 1 at 0.8 of y axis in outer margins for all sides
for(side in 1:4)
  mtext(text = paste("Line", -1:1), side = side, at = 0.8,
        line = -1:2, col = "blue", outer = TRUE)
# "Plot Region" placed at 0, 12
text(x = 0, y = 12, "Plot Region", cex = 3, col = "gray40")
# Red solid circle placed at 0, 4
points(x = 0, y = 4, pch = 19, col = "red", cex = 2)
# "(0, 4)" placed centered (0.5) and 2 lines below 0, 4
text(x = 0, y = 4, "(0, 4)", adj = c(0.5, 2), cex = 1.5)
# text with math symbols created with expression placed at (0, 7.5)
text(x = 0, y = 7.5, expression(paste("Some Math: ", bar(x)==
                                        sum(frac(x[i], n), i==1, n))), cex = 1.5, col = "blue")
# "x-label" placed at  side 1 (bottom) line 3 of figure margin
mtext(text = "x-label", side = 1, line = 3)
# "subtitle" placed at side 1 (bottom) line 4 of figure margin
mtext(text = "subtitle", side = 1, line = 4)
# "y-label" placed at side 3 (top) line 3 of figure margin
mtext(text = "y-label", side = 2, line = 3)
# "Title" placed at side 3 (top) line 2 of figure margin
mtext(text = "Title", side = 3, line = 2, cex = 1.25)
# "0%" placed at side 3 (top) line 0.5 at x = 0 of outer margin
mtext(text = "0%", side = 3, line = 0.5, outer = TRUE, at = 0)
# "100%" placed at side 3 (top) line 0.5 at x = 1 of outer margin
mtext(text = "100%", side = 3, line = 0.5, outer = TRUE, at = 1)
# "(line = 2, at = 5)" placed at side 1 (bottom) line 2 of figure margin
mtext(text = "(line = 2, at = 4.5)", side = 1, line = 2, at = 4.5)
# "(line = 0.5, at = 0.2)" placed at x = 0.2 on side 1 (bottom) line 0.5
# of the outer margin
mtext(text = "(line = 0.5, at = 0.2)", side = 1, line = 0.5, at = 0.2,
      outer = TRUE)

################### R code 1.69

# figure margins of 2.2, 2.2, 0.2, and 0.2 lines
par(mar=c(2, 2, 0, 0) + 0.2)
plot(x = 1, y = 1, xlim = c(1, 16), ylim = c(-1.5, 5), type = "n",
     xlab = "", ylab = "")  # create empty plot with x and y axes
COLORS <- c("black", "red", "green", "darkblue", "darkgreen",
            "magenta", "orange", "cyan")  # vector of colors
# symbols (pch = 0:7) placed at (1, 4), (3, 4), ...(15, 4) with
# character expansion 1:8 with color specified in COLORS
points(x = seq(1, 15, 2), y = rep(4, 8), cex = 1:8, col = COLORS,
       pch = 0:7, lwd = 2)
# labels 0:7 placed at (1, 2), (3, 2),..., (15, 2) with
# character expansion 1:8 with color specified in COLORS
text(x = seq(1, 15, 2), y = rep(2, 8), labels = paste(0:7), cex = 1:8,
     col = COLORS)
# symbols (pch = 8:15) placed at (1, 0), (3, 0),..., (15, 0)
# with character expansion of 2
points(x = seq(1, 15, 2), y = rep(0, 8), pch = 8:15, cex = 2)
# labels 8:15 placed 0.7 to the right of (1, 0), (3, 0),..., (15, 0)
# with character expansion of 2
text(x = seq(1, 15, 2) + 0.7, y = rep(0, 8), labels = paste(8:15),
     cex = 2)
# symbols (pch = 16:23) placed at (1, -1), (3, -1),..., (15, -1)
# with character expansion of 2
points(x = seq(1, 15, 2), y = rep(-1, 8), pch = 16:23, cex = 2)
# labels 16:23 placed 0.7 to the right of (1, -1), (3, -1),..., (15, -1)
# with character expansion of 2
text(x = seq(1, 15, 2) + 0.7, y = rep(-1, 8), labels = paste(16:23),
     cex = 2)

################### R code 1.70

opar <- par(no.readonly = TRUE)  # copy of current settings
par(mar=c(2, 4, 2, 1), lty = 3, bg = "gray", las = 3, ...) # par changes
plot(x)                          # plot x with changed parameters
par(opar)                        # restore original settings