## PARITALA GOURI 
## 19BDS0162
## PRACTICE - SCALING AND ENCODING

## THE DATA-SET USED -> student-mat.csv

## Data-set columns used in the code -> descrition
## school - student's school (binary: "GP" - Gabriel Pereira or "MS" - Mousinho da Silveira)
## activities - extra-curricular activities (binary: yes or no)
## paid - extra paid classes within the course subject (binary: yes or no)
## reason - reason to choose this school (nominal: close to "home", school "reputation", "course" preference or "other")
## guardian - student's guardian (nominal: "mother", "father" or "other")
## G3 - final grade (numeric: from 0 to 20, output target)
## age - student's age (numeric: from 15 to 22)

## --------------------------------------------------

library(base)
library(utils)
library(jsonlite)
library(httr)
library(stats)
library(graphics)
library(caret)

## READ THE DATA-SET:

df <- read.csv("https://raw.githubusercontent.com/Gouri-19/Datasets/main/student-mat.csv")

View(df)
str(df)

## ENCODING
## LABEL ENCODING
## Label encoding for the columns - school, activities, paid

## For school and activities column;

df$school <- factor(df$school, levels = c("GP","MS"), labels = c(1,2))
df$activities <- factor(df$activities, levels = c("no", "yes"), labels = c(0,1))
head(df)

## For paid column, creating a new column and label encoding it;

df$label_paid <- factor(df$paid, levels = c("no", "yes"), labels = c(0,1))
head(df)

## ONE HOT ENCODING
## One hot encoding for the columns -> reason, guardian

dmy <- dummyVars(" ~ reason+guardian", data = df)
one_enc <- data.frame(predict(dmy, newdata = df))

(one_enc)

## SCALING
## MIN-MAX NORMALIZATION
## Normalization on the column -> G2 and G3

min_max_normalization <- function(x){
  (x-min(x))/(max(x)-min(x))
}

normalized_G3 <- min_max_normalization(df$G3)
(normalized_G3)

hist(normalized_G3, breaks = 10, xlab = "Normalized Data", col = "lightgreen")


## Z-SCALE STANDARDIATION
## Standardization on the column -> age

zscale_standardization <- function(x){
  (x-mean(x))/sd(x)
}

standardized_age <- zscale_standardization(df$age)
(standardized_age)

hist(standardized_age, breaks = 10, xlab = "Normalized Data", col = "lightblue")



