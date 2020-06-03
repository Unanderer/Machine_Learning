#############################################################################
#
#   Movie Genre Prediction Using NLP and Sentiment Analysis
#   05/06/2019
#   Hunter Martin
#
#############################################################################

install.packages("sentimentr")
install.packages("tidytext")
install.packages("factoextra")
library(sentimentr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(MASS)
library(glmnet)
library(Metrics)
library(factoextra)

## reading in the moives datasetr
movies <- read.csv("movies_prepped.csv")


# For some reason input file had 1701 columns, so got rid of all the blank columns
movies <- movies[, 1:32]
names(movies)
# remove columns adult, homepage, poster_path, crew, and original language, and video
movies <- movies[, c(-1, -2, -5, -12, -26)]
movies <- movies[,-5]
movies <- movies[,-18]


# Question 3 #####################################################################################################
# Creating Keywords Dataframe
keywords_analysis <- movies[, c(2, 5, 6, 7, 15, 20, 21, 25)]
# removing brackets from the list of keywords and genres
keywords_analysis$keywords <-
  gsub("\\[|\\]", "", keywords_analysis$keywords)
keywords_analysis$keywords <-
  gsub("\\'|\\'", "", keywords_analysis$keywords)
keywords_analysis$genres <-
  gsub("\\[|\\]", "", keywords_analysis$genres)
keywords_analysis$genres <-
  gsub("\\'|\\'", "", keywords_analysis$genres)

# Filling blanks with NAs
keywords_analysis[keywords_analysis == ""] <- NA
keywords_analysis[keywords_analysis == " "] <- NA
summary(is.na(keywords_analysis))

# Getting rid of movies that have an NA value
keywords_analysis <-
  keywords_analysis[complete.cases(keywords_analysis), ]

## Making keywords data frame contain genres, title, keywords (called sentence), and weighted rating
keywords_analysis <-
  data.frame(
    keywords_analysis$genres,
    keywords_analysis$original_title,
    keywords_analysis$weighted_rating,
    keywords_analysis$revenue_divide_budget,
    sentence = keywords_analysis$keywords,
    stringsAsFactors = FALSE
  )
# converting sentence to a character from a factor
keywords_analysis$sentence <-
  as.character(keywords_analysis$sentence)

# creating a backup keywords dataframe before we do any further manipulation
keywords_analysis_forlater <-
  keywords_analysis[complete.cases(keywords_analysis), ]







### Different emotion scores ##########################################################################################

## Getting all emotion lexicons for NRC
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

nrcanger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

nrcfear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")

nrctrust <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")

nrcanticipation <- get_sentiments("nrc") %>%
  filter(sentiment == "anticipation")

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

nrcpositive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

nrcnegative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")

# Getting joy score
for (i in 1:nrow(keywords_analysis)) {
  # selects just the title and the keywords and calling it row
  row <- keywords_analysis[i, c(2, 5)]
  # makes sure sentence is of type character
  row$sentence <- as.character(row$sentence)
  # changing title to type character because it was causing errors
  row$keywords_analysis.original_title <-
    as.character(row$keywords_analysis.original_title)
  # unnesting each word in sentene so that each word has its own row
  row <- row %>%
    unnest_tokens(sentence, sentence)
  # joining joy words to row data frame
  joy_words <- row %>%
    inner_join(nrcjoy, by = c("sentence" = "word"))
  # counting total words in sentence
  total_words <- row %>%
    count(sentence)
  # dividing count of joy_words by a count of total_words to get joy_score
  joy_score <- nrow(joy_words) / nrow(total_words)
  keywords_analysis$joy[i] <- joy_score
}

# Getting anger score
for (i in 1:nrow(keywords_analysis)) {
  row <- keywords_analysis[i, c(2, 5)]
  row$sentence <- as.character(row$sentence)
  row$keywords_analysis.original_title <-
    as.character(row$keywords_analysis.original_title)
  row <- row %>%
    unnest_tokens(sentence, sentence)
  anger_words <- row %>%
    inner_join(nrcanger, by = c("sentence" = "word"))
  total_words <- row %>%
    count(sentence)
  anger_score <- nrow(anger_words) / nrow(total_words)
  keywords_analysis$anger[i] <- anger_score
}

# Getting fear score
for (i in 1:nrow(keywords_analysis)) {
  row <- keywords_analysis[i, c(2, 5)]
  row$sentence <- as.character(row$sentence)
  row$keywords_analysis.original_title <-
    as.character(row$keywords_analysis.original_title)
  row <- row %>%
    unnest_tokens(sentence, sentence)
  fear_words <- row %>%
    inner_join(nrcfear, by = c("sentence" = "word"))
  total_words <- row %>%
    count(sentence)
  fear_score <- nrow(fear_words) / nrow(total_words)
  keywords_analysis$fear[i] <- fear_score
}

# Getting trust score
for (i in 1:nrow(keywords_analysis)) {
  row <- keywords_analysis[i, c(2, 5)]
  row$sentence <- as.character(row$sentence)
  row$keywords_analysis.original_title <-
    as.character(row$keywords_analysis.original_title)
  row <- row %>%
    unnest_tokens(sentence, sentence)
  trust_words <- row %>%
    inner_join(nrctrust, by = c("sentence" = "word"))
  total_words <- row %>%
    count(sentence)
  trust_score <- nrow(trust_words) / nrow(total_words)
  keywords_analysis$trust[i] <- trust_score
}

# Getting anticipation score
for (i in 1:nrow(keywords_analysis)) {
  row <- keywords_analysis[i, c(2, 5)]
  row$sentence <- as.character(row$sentence)
  row$keywords_analysis.original_title <-
    as.character(row$keywords_analysis.original_title)
  row <- row %>%
    unnest_tokens(sentence, sentence)
  anticipation_words <- row %>%
    inner_join(nrcanticipation, by = c("sentence" = "word"))
  total_words <- row %>%
    count(sentence)
  anticipation_score <- nrow(anticipation_words) / nrow(total_words)
  keywords_analysis$anticipation[i] <- anticipation_score
}

# Getting surprise score
for (i in 1:nrow(keywords_analysis)) {
  row <- keywords_analysis[i, c(2, 5)]
  row$sentence <- as.character(row$sentence)
  row$keywords_analysis.original_title <-
    as.character(row$keywords_analysis.original_title)
  row <- row %>%
    unnest_tokens(sentence, sentence)
  surprise_words <- row %>%
    inner_join(nrcsurprise, by = c("sentence" = "word"))
  total_words <- row %>%
    count(sentence)
  surprise_score <- nrow(surprise_words) / nrow(total_words)
  keywords_analysis$surprise[i] <- surprise_score
}

# Getting positive score
for (i in 1:nrow(keywords_analysis)) {
  row <- keywords_analysis[i, c(2, 5)]
  row$sentence <- as.character(row$sentence)
  row$keywords_analysis.original_title <-
    as.character(row$keywords_analysis.original_title)
  row <- row %>%
    unnest_tokens(sentence, sentence)
  positive_words <- row %>%
    inner_join(nrcpositive, by = c("sentence" = "word"))
  total_words <- row %>%
    count(sentence)
  positive_score <- nrow(positive_words) / nrow(total_words)
  keywords_analysis$positive[i] <- positive_score
}

# Getting negative score
for (i in 1:nrow(keywords_analysis)) {
  row <- keywords_analysis[i, c(2, 5)]
  row$sentence <- as.character(row$sentence)
  row$keywords_analysis.original_title <-
    as.character(row$keywords_analysis.original_title)
  row <- row %>%
    unnest_tokens(sentence, sentence)
  negative_words <- row %>%
    inner_join(nrcnegative, by = c("sentence" = "word"))
  total_words <- row %>%
    count(sentence)
  negative_score <- nrow(negative_words) / nrow(total_words)
  keywords_analysis$negative[i] <- negative_score
}

# Exporting the table with sentiment scores added, so we do not have to re-run code every time we want to do
# analysis. Can just read in the new table and start from there.
write.csv(keywords_analysis, "final_table.csv")
keywords_analysis <-
  read.csv("final_table.csv",
           header = TRUE,
           stringsAsFactors = FALSE)

# converting factor columns to numeric for analysis
keywords_analysis$keywords_analysis.revenue_divide_budget <-
  as.numeric(as.character(keywords_analysis$keywords_analysis.revenue_divide_budget))
keywords_analysis$keywords_analysis.weighted_rating <-
  as.numeric(as.character(keywords_analysis$keywords_analysis.weighted_rating))

# Removing invalid rows that did not get removed in initial cleaning
keywords_analysis <-
  keywords_analysis[-c(
    4466,
    4513,
    2846,
    4515,
    4208,
    1786,
    2265,
    3998,
    1344,
    2091,
    3476,
    3819,
    4464,
    1527,
    1810,
    4522
  ), ]

# Plotting to see relationship between sentiments and rating
plot(keywords_analysis$joy,
     keywords_analysis$keywords_analysis.weighted_rating)
plot(keywords_analysis$anger,
     keywords_analysis$keywords_analysis.weighted_rating)
plot(keywords_analysis$surprise,
     keywords_analysis$keywords_analysis.weighted_rating)
plot(
  keywords_analysis$anticipation,
  keywords_analysis$keywords_analysis.weighted_rating
)
plot(keywords_analysis$positive,
     keywords_analysis$keywords_analysis.weighted_rating)
plot(keywords_analysis$negative,
     keywords_analysis$keywords_analysis.weighted_rating)

# Checking variables for skewness and need for transformation
hist(keywords_analysis$joy)
hist(keywords_analysis$anger)
hist(keywords_analysis$anticipation)
hist(keywords_analysis$surprise)
hist(keywords_analysis$positive)
hist(keywords_analysis$negative)
hist(keywords_analysis$fear)
hist(keywords_analysis$trust)
hist(keywords_analysis$keywords_analysis.weighted_rating)


# sqrt transformed because sentiment scores were right skewed
keywords_analysis$joy <- sqrt(keywords_analysis$joy)
keywords_analysis$anger <- sqrt(keywords_analysis$anger)
keywords_analysis$fear <- sqrt(keywords_analysis$fear)
keywords_analysis$trust <- sqrt(keywords_analysis$trust)
keywords_analysis$anticipation <-
  sqrt(keywords_analysis$anticipation)
keywords_analysis$surprise <- sqrt(keywords_analysis$surprise)
keywords_analysis$positive <- sqrt(keywords_analysis$positive)
keywords_analysis$negative <- sqrt(keywords_analysis$negative)

# Linear Regression on rating ###############################################################################################
# regression with genre not included
rating_reg_NOG <- keywords_analysis[, c(3, 6, 7, 8, 9, 10, 11, 12, 13)]
rating_reg_NOG$keywords_analysis.weighted_rating <-
  as.numeric(as.character(rating_reg_NOG$keywords_analysis.weighted_rating))
rating_regred_NOG <-
  lm(keywords_analysis.weighted_rating ~ ., data = rating_reg_NOG)
summary(rating_regred_NOG)

#regression with genre included
rating_reg <- keywords_analysis[, c(1, 3, 6, 7, 8, 9, 10, 11, 12, 13)]
rating_reg$keywords_analysis.weighted_rating <-
  as.numeric(as.character(rating_reg$keywords_analysis.weighted_rating))
rating_regred <-
  lm(keywords_analysis.weighted_rating ~ ., data = rating_reg)
summary(rating_regred)


# Feature Selection for rating WITH genre
step1 <-
  stepAIC(rating_regred_NOG,
          direction = "both",
          na.action = na.remove)
step1$anova
# got rid of trust, fear, anticipation

# regression on final model
final_reg_rating <-
  lm(keywords_analysis.weighted_rating ~ joy + anger + surprise +
       positive + negative,
     data = rating_reg_NOG)
summary(final_reg_rating)


# K-Means ##################################################################################
# Finding the optimal number for k
kmeans_table <- keywords_analysis[, c(3, 6, 7, 8, 9, 10, 11, 12, 13)]
k_max <- 15
sum_sq <- sapply(1:k_max, function(k) {
  kmeans(kmeans_table, k, nstart = 50, iter.max = 15)$tot.withinss
})
plot(1:k_max,
     sum_sq,
     type = "b",
     pch = 1,
     xlab = "K")

# trial and error on different values of k. K=4  was optimal by elbow mehtod, but decided to go with k=8
model8_kmeans <- kmeans(kmeans_table, 8, nstart = 50)
model10_kmeans <- kmeans(kmeans_table, 10, nstart = 50)
model4_kmeans <- kmeans(kmeans_table, 4, nstart = 50)

#binding results to main data frame
train_kmeans <- cbind(keywords_analysis, model8_kmeans$cluster)
train_kmeans <- cbind(keywords_analysis, model10_kmeans$cluster)
train_kmeans <- cbind(keywords_analysis, model4_kmeans$cluster)

#getting results of what is actually in clusters
library(psych)
describeBy(keywords_analysis, model8_kmeans$cluster)
describeBy(keywords_analysis, model4_kmeans$cluster)
describeBy(keywords_analysis, model10_kmeans$cluster)

# Getting a visual representation of the clusters for k=8
fviz_cluster(model8_kmeans, data = kmeans_table, geom = "point")