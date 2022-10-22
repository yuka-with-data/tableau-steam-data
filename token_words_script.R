# Create A Steam Word Data For A Word Cloud
# Removed StopWords, Outliers and Common Words

#----------------------------------------------------
# install missing packages and load required packages
#----------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
library(scales)
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
library(tidytext)
if(!require(textdata)) install.packages("textdata", repos = "http://cran.us.r-project.org")
library(textdata)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
library(stringr)
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
library(ggthemes)
if(!require(sentimentr)) install.packages("sentimentr", repos = "http://cran.us.r-project.org")
library(sentimentr)
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
library(DT)
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
library(gam)
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
library(rpart)
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
library(rmarkdown)
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)
if(!require(pins)) install.packages("pins", repos = "http://cran.us.r-project.org")
library(pins)
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
library(devtools)
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
library(psych)
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
library(e1071)
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
library(xgboost)

#----------------------------------------------------
# Data Preparation
#----------------------------------------------------

# The original data was provided by Kaggle Contributor and ML Engineer at ByteDance, Luthfi Mahendra.
# Data Source: https://www.kaggle.com/luthfim/steam-reviews-dataset

##### download a data file manually from Kaggle.
# download zip file from
# https://www.kaggle.com/luthfim/steam-reviews-dataset/download
# open a zip file, and save steam_review.csv file in your working directly
steam_reviews <- read_csv("steam_reviews.csv")
# Double check to save the file as "steam_reviews".


#-------- Create Validation (Final Hold-Out) Set ---------
# set seed for 2022
set.seed(2022, sample.kind = "Rounding")
# Spread the steam_reviews data set into 2 parts: 
# 40% for Validation, and 60% for train and test data set to train algorithms
test_index <- createDataPartition(y = steam_reviews$recommendation, 
                                  times = 1, 
                                  p = 0.4, 
                                  list = FALSE)
steam_main_data <- steam_reviews[-test_index,]
validation <- steam_reviews[test_index,]
# To make sure not to include recommendation in the validation set 
# that do not appear in the steam_main_data set
validation <- validation %>% 
  semi_join(steam_main_data, by = "recommendation")
# Open steam_main_data frame
View(steam_main_data)

#----------------------------------------------------
# Data Preparation
#----------------------------------------------------

## Outlier Removal
#--- The 'funny' variable contains some outliers, 
#--- and are located in both steam_main_data and validation data set.
#--- These observations will be removed.
# Retrieve row numbers of outliers
funny_outlier <- which(steam_main_data$funny %in% c(4294967295, 4294967294, 4294967288))
funny_outlier # in steam_main_data
funny_outlier_val <- which(validation$funny %in% c(4294967295, 4294967294, 4294967293))
funny_outlier_val # in validation
# Remove outliers from both steam_main_data and validation set
steam_main_data <- steam_main_data %>% 
  dplyr::slice(-funny_outlier)


# Remove Stop Words
#data("stop_words")
#token_words <- main_rgx %>% unnest_tokens(word, new_review) %>% anti_join(stop_words)

# Remove Stop Words
data("stop_words")
steam_main_data <- steam_main_data %>% unnest_tokens(word, review) %>% anti_join(stop_words)


## Restructuring Data Prior To Training Classification Algorithms

# 19 patterns of regex to remove or replace 
# certain words and abbreviations in review data
# non-English foreign language
rgx1 <- "[^a-zA-Z0-9\\s[:punct:]]+" 
# game
rgx3 <- "[g|G][a|A][m|M][e|E]"
# games
rgx4 <- "[g|G][a|A][m|M][e|E][s|S]"
# xml grater
rgx5 <- "[&][g][t]\\s" #might NOT remove "gt"
### "https?://(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()!@:%_\\+.~#?&//=]*)" 
# would be the correct URL regex. 
# However, the data I am working with does not include : but instead white space, 
# so I adjusted my regex for this project.
rgx6 <- "https?\\s//(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()!@:%_\\+.~#?&//=]*)"
# ggwp
rgx18 <- "[g|G][g|G][w|W][p|P]"
# Replace "gg" with 3 sets of regex
rgx8 <- "\b[g|G]{2}\\s"
rgx9 <- "\\s[g|G]{2}\b"
rgx16 <- "\\s[g|G]{2}\\s"
rgx17 <- "[G]{2}"
# rekt
rgx19 <- "[r|R][e|E][k|K][t|T]"
# Replace with good
rgx10 <- "[g|G][u|U][d|D]"
# Replace with love
rgx11 <- "\b[l|L][u|U][v|V]\b"
# Replace ratings
rgx12 <- "[1][1]/[1][0]" # "Great"
rgx13 <- "[1][0]/[1][0]" # "Good"
rgx14 <- "[1]/[1][0]" # "Worst"
# remove the rest of punctuation and digits at the end
rgx15 <- "[:punct:]"
rgx2 <- "[0-9]+"

# Replace and remove words using regex
# words and symbol removal
main_rgx <- str_replace_all(steam_main_data$word, rgx1, "") %>% as.data.frame()
main_rgx <- str_replace_all(main_rgx$., rgx4, "") %>% as.data.frame()
main_rgx<- str_replace_all(main_rgx$., rgx5, " ") %>% as.data.frame()
main_rgx <- str_replace_all(main_rgx$., rgx6, "") %>% as.data.frame()
# replace ggwp with "good game well played"
main_rgx <- str_replace_all(main_rgx$., rgx18, " good game well played ") %>% as.data.frame()
# replace gg with "good game" 
main_rgx <- str_replace_all(main_rgx$., rgx8, " good game ") %>% as.data.frame()
main_rgx <- str_replace_all(main_rgx$., rgx9, " good game ") %>% as.data.frame()
main_rgx <- str_replace_all(main_rgx$., rgx16, " good game ") %>% as.data.frame()
main_rgx <- str_replace_all(main_rgx$., rgx17, " good game ") %>% as.data.frame()
# replace gud with "good"
main_rgx <- str_replace_all(main_rgx$., rgx10, " good ") %>% as.data.frame()
# replace luv with "love"
main_rgx <- str_replace_all(main_rgx$., rgx11, " love ") %>% as.data.frame()
# replace rekt with "wrecked"
main_rgx <- str_replace_all(main_rgx$., rgx19, "wrecked ") %>% as.data.frame()
# Replace Ratings
# 11/10 with "Excellent"
main_rgx <- str_replace_all(main_rgx$., rgx12, " Excellent ") %>% as.data.frame()
# 10/10 with "Good"
main_rgx <- str_replace_all(main_rgx$., rgx13, " Good ") %>% as.data.frame()
# 1/10 with "Worst"
main_rgx <- str_replace_all(main_rgx$., rgx14, " Worst ") %>% as.data.frame()

# Remove Digits and Punctuation
main_rgx <- str_replace_all(main_rgx$., rgx15, "") %>% as.data.frame()
main_rgx <- str_replace_all(main_rgx$., rgx2, "") %>% as.data.frame()
# Remove game at the end
main_rgx <- str_replace_all(main_rgx$., rgx3, "") %>% as.data.frame()

# change the column name
main_rgx <- rename(main_rgx, new_word = .)

# remove missing values in rows and save it in a new variable
token_words <- main_rgx %>% drop_na()

#----------------------------------------------------
# How To Download the dataframe to your directory
#----------------------------------------------------
# Use write.csv function from base R to export the dataframe to CSV file
# Include the dataframe, and your file path + 

# write.csv(token_words, ""C:\\Users\\your_file_path\\token_words_df.csv", row.name = FALSE")

#----------------- END ------------------------