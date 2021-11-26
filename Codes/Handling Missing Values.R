## PARITALA GOURI 
## 19BDS0162
## HANDLING MISSING DATA VALUES
##---------------------------------------------------

# Required Libraries 

library(base)
library(utils)
library(jsonlite)
library(httr)
library(naniar)
library(imputeMissings)
library(VIM)

# Read the data-set.

df <- read.csv("https://raw.githubusercontent.com/Gouri-19/Datasets/main/Titanic.csv")
View(df)

# Check if there are any missing data.

vis_miss(df)

# FIRST TASK/QUESTION
# Remove the rows/columns having the missing data more than 60%.
# In this data-set, the column "Age" has more than 60% NA values.

dfs <- df[ ,which(colMeans(!is.na(df)) > 0.6)]
View(dfs)

# SECOND TASK/QUESTION
# Missing value - Median Imputation.
# Using Median - Imputing Missing Values for the remaining columns. 

dfs <- impute(dfs, method = "median/mode")
head(dfs)

# To view the entire data after imputing missing values.

vis_miss(dfs)

# THIRD TASK/QUESTION
# Standard Algorithm

# KNN algorithm - Missing Value Imputation.
# kNN algorithm can be much more accurate than the mean or median.
# The algorithm uses 'feature similarity' to predict the values of any new data points.
# It helps in making predictions about the missing values by finding the k's closest neighbours to the observation with missing data.
# and then imputing them based on the non-missing values in the neighbourhood. 
# kNN-alogrithm is not only used to fix the missing values for categorical variables(columns) but also numerical variables.

# Required libraries - library(VIM).
# I have used the original data-set to show how missing values are imputed in a data-set.
# Firstly read the data-set again and for the rows/column having more that 60% of NA values are removed.

df1 <- read.csv("Titanic.csv")
View(df1)

dfd <- df1[ ,which(colMeans(!is.na(df1)) > 0.6)]
View(dfd)

# Secondly, for imputing the missing values in rest of the columns ,I have used the kNN algorithm.
# The rest of the columns that need imputing are; Sibsp, Parch, Survived, Pclass. 
# These rest of the variables(columns) have NA values present.

dfa <- kNN(dfd, variable = c("Survived", "Parch", "SibSp", "Pclass"))
(dfa)

# By default, k=5.
# To view the data.

summary(dfa)
head(dfa)

# Once till here the code has been executed, the missing values will get imputed.
# But the algorithm also produces a few extra variables like; Survived_Imp, SibSp_Imp, Parch_Imp and  Pclass_Imp which are logical in nature.
# kNN uses these variables for filling the missing values.

# We can remove those extra variables(columns) as they aren't required in the data-set.

dfa <- subset(dfa, select = PassengerId:Embarked)

summary(dfa)
head(dfa)

# To view the entire data after imputing missing values.

vis_miss(dfa)
