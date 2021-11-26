## PARITALA GOURI
## 19BDS0162
## COLLECTING DATA USING AN API
##----------------------------------------------------------------------------

# Required Libraries

library(jsonlite)
library(httr)


# Website : Gnew API
# API CALL URL: https://gnews.io/api/v4/top-headlines?lang=en&country={example}&topic={example}&token=API-Token
# API KEY: 1ccddb4a24220d543302fe6961c03ba1

# Accessing Web Data using an API

url = "https://gnews.io/api/v4/top-headlines?lang=en&topic=sports&token=1ccddb4a24220d543302fe6961c03ba1"

# Convert JSON into Data Frame

content = fromJSON(url)

# Remove the columns that aren't required

content$articles$content <- NULL
content$articles$url <- NULL
content$articles$image <- NULL

# Parse and store news article information in CSV File

write.table(content$articles, file="Gnews_19BDS0162.csv", sep=",", append=TRUE, row.names=FALSE, col.names = FALSE)

