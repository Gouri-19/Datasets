# PARITALA GOURI 
# 19BDS0162
# STATISTICAL METHODS AND HYPOTHESIS TESTING
##-------------------------------------------------------------------------------------

## STATISTICAL METHODS

# Required Libraries

library(base)
library(utils)
library(stats)
library(jsonlite)
library(httr)
library(modeest)
library(imputeMissings)
library(graphics)

# Read the data-set and impute the missing values to do the required tasks.

df <- read.csv("https://raw.githubusercontent.com/Gouri-19/Datasets/main/Titanic.csv")
View(df)

df <- df[ ,which(colMeans(!is.na(df)) > 0.6)]
View(df)

# Imputing missing values in the data-set.

df <- impute(df, method="median/mode")

View(df)
str(df)

# Take one particular variable and do the required statistical methods.

dfd <- df$Fare

summary(dfd)

# The column - Fare is an atomic vector.

## USER - DEFINED FUNCTIONS.

## FIND MEAN
# Here, mean is calculated by taking the sum of the values in the variable(column) called Fare
# and divided by total number of values(observations) in the column.

mymean <- function(dfd) {
  n1 <- length(dfd)
  s1 <- sum(dfd)
  m1 <- s1/n1
  (m1)
}
mymean(dfd)

## FIND MEDIAN
# Firstly sort the values(observations) in the column and then calculate the median.

mymed <- function(dfd) {
  n2 <- length(dfd)
  s2 <- sort(dfd)
  ifelse(n2%%2==1,s2[(n2+1)/2],mymean(s2[n2/2+0.1]))
}
mymed(dfd)

## FIND MODE
# Calculating the most frequent value of the column - Fare.

mymode <- function(dfd) {
  m2 <- unique(dfd)
  tab <- tabulate(match(dfd,m2))
  m2[tab==max(tab)]
}
mymode(dfd)

## FIND IQR
## IQR <- Inter-Qaurtile Range.
## IQR is defined as the difference between first(lower) quartile and third(upper) quartile.
## To check if the function is correct or not.

myiqr <- function(dfd){
  q1 <- quantile(dfd, 0.25)
  q3 <- quantile(dfd, 0.75)
  (iqr1 <- q3-q1)
}
myiqr(dfd)

## FIND STANDARD DEVIATION.
# Standard deviation is calculated by taking the square root of sum(square(x-mean)) divided by (total observations - 1).

mysd <- function(dfd) {
  n3 <- length(dfd)
  sd1 <- sqrt(sum((dfd-mymean(dfd))^2) / (n3-1))
  (sd1)
}
mysd(dfd)

## FIND PROBABILITY VALUES ON EMPIRICUL RULE.
# It is also known as the 68-95-99.7 rule.
# 68% of observed data points will lie inside 1 sd of the mean. 
# 95% will fall within 2 sd
# 99.7% will occur within 3 sd.

myempr <- function(dfd) {
  m4 <- mymean(dfd)
  m5 <- mysd(dfd)
  empr1 <- c(((m4)-(1*m5)),((m4)+(1*m5)))
  empr2 <- c(((m4)-(2*m5)),((m4)+(2*m5)))
  empr3 <- c(((m4)-(3*m5)),((m4)+(3*m5)))
  print("Probability Values - Empirical Rule")
  print(empr1)
  print(empr2)
  print(empr3)
}
myempr(dfd)

## PLOT THE GRAPH/HISTOGRAM/NORMAL DISTRIBUTION.
## Histogram

hist(dfd)

# Numeric Histogram Frequencies

table(dfd)

# Normal Distribution Curve

hist(dfd, probability = TRUE)
curve(dnorm(x, mymean(dfd), mysd(dfd)), add = TRUE)

## COMPARING USER DEFINED FUNCTION FOR MEAN, MEDIAN, MODE, SD, IQR WITH PREDEFINED FUNCTIONS.
## FOR MEAN

mean(dfd)
mymean(dfd)
# They are equal.

## FOR MEDIAN

median(dfd)
mymed(dfd)
# They are equal.

## FOR MODE

mfv(dfd)
mymode(dfd)
# They are equal.

##FOR IQR
IQR(dfd)
myiqr(dfd)
##They are equal.

## FOR SD

sd(dfd)
mysd(dfd)
# They are equal.

## HYPOTHESIS TESTING

# Data-set -> student-mat.csv
# This data-set is going to be used for hypothesis testing.
# This data-set contains around 33 variables(columns) and 395 observations(rows)

# Read the data-set. 

df <- read.csv("https://raw.githubusercontent.com/Gouri-19/Datasets/main/student-mat.csv")
View(df)

head(df[c(1,2,3,15,19,30)])

# Data Description for the columns chosen;
# school - student's school (binary: 'GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)
# sex - student's sex (binary: 'F' - female or 'M' - male)
# age - student's age (numeric: from 15 to 22)
# failures - number of past class failures (numeric: n if 1<=n<3, else 4)
# activities - extra-curricular activities (binary: yes or no)
# absences - number of school absences (numeric: from 0 to 93)

# Data-set Summary:

str(df)

# Null Hypothesis -> H0
# Alternate Hypothesis -> Ha

# H0 -> There is no significance difference between female and male students with respect to number of absences.
# Ha would be -> There is a significance difference between female and male students with respect to number of absences.

# Read Female and Male Absences list:

dfm <- df[df$sex == "M", c(2,30)]
dff <- df[df$sex == "F", c(2,30)]

str(dff)
str(dfm)

head(dfm)
head(dff)

# Applying t-test:

result <- t.test(sample(dfm$absences, 100), sample(dff$absences, 100), var.equal = TRUE)
(result)

## Observation of the Output:
# df -> degrees of freedom
# significance value -> alpha = 0.05
# P-value = 0.1841 
# P-value > alpha value

## Conclusion:
# As P-value is greater than alpha value, null hypothesis (H0) wont be rejected.
# H0 -> There is no significance difference between female and male students with respect to the number of absences.
# Based on the mean value, the female students have got higher absences when compared to male students.

