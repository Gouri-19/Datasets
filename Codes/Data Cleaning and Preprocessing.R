## PARITALA GOURI
## 19BDS0162
## DATA CLEANING AND PRE-PROCESSING
##---------------------------------------------

# Required Libraries

library(jsonlite)
library(httr)
library(base)
library(utils)

# Reading the csv file from github

df <- read.csv("https://raw.githubusercontent.com/anthoniraj/datasets/main/data_cleaning/tweet.csv")
head(df)

# Remove the misplaced, empty rows and NA columns

dfs <- c("X","X.1","X.2");
df <- df[, !(names(df) %in% dfs)]
head(df)

# Retrieve only rows contains twitter name from text column

df <-filter(df, grepl('\\@(.*?)\\:', text, ignore.case =T))

# Convert re-tweet column into numeric format

df$retweetCount <- as.integer(as.character(df$retweetCount))
head(df)

# Filter the subset with re-tweet count is greater than 1000

df <- subset(df, retweetCount > 1000)
head(df)

# Convert created into data and time object

df[['created']] <- strptime(df[['created']], format ="%d-%m-%Y %H:%M")
head(df)

# Extract username from the text column and rebuild the data frame

df$text <- str_extract(df$text,'\\@(.*?)\\:') %>%
  str_sub(1, -2)
head(dfd)

# Extract Text from HTML script from statusSource column and rebuild the data frame 

p_text <-function(source){
  
  html <-minimal_html(source)
  content <-html_elements(html, "a")
  return(html_text(content))
  }

df$statusSource <-lapply(df$statusSource, p_text)

head(df)
