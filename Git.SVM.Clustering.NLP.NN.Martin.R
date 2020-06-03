

#############################################################################
#
#   SVM.Clustering.NLP.NN
#   05/16/2019
#   Hunter Martin
#
#############################################################################

# removed for privacy reasons
setwd("PATH/TO/DATA")


install.packages("tree")
install.packages("randomForest")
install.packages("gbm")
installed.packages("Metrics")
install.packages("mlbench")
install.packages("wordcloud")
install.packages("klaR")
install.packages("nnet")
install.packages("doSNOW")
install.packages("factoextra")

library(corrplot)  # graphical display of the correlation matrix
library(caret) # classification and regression training
library(klaR)      # naive bayes
library(nnet)      # neural networks (nnet and avNNet)
library(kernlab)   # support vector machines (svmLinear and svmRadial)
library(randomForest)  # random forest, also for recursive feature elimination
library(gridExtra) # save dataframes as images
library(factoextra)


# Reads data
data <- read.csv(file = "winequality.csv", header = TRUE, sep = ",")
#Extracts ID fields
data <- data[, -1]
# Sets seed
set.seed(123)

type = ifelse(data$type == 1, "White", "Red")
data$type <- NULL
# appends factor to data
data <- data.frame(data, type)



#Split into Training and Test sets

index <- sample(1:nrow(data), size = 0.7 * nrow(data))
train <- data[index, ]
test <- data[-index, ]



#####################   SVM

library(e1071)


#linear
type_model <- svm(type ~ ., data = data, kernel = "linear")
summary(type_model)
pred <- predict(type_model, data)
tab1 <- table(Predicted = pred, Actual = data$type)
tab1

tab1[2, 2] / (tab1[2, 1] + tab1[2, 2])
#Accuracy of 0.9963288
fourfoldplot(tab1)
1 - sum(diag(tab1)) / sum(tab1)
#Misclassification of 0.4771%

#polynomial
type_model <- svm(type ~ ., data = data, kernel = "polynomial")
summary(type_model)
pred <- predict(type_model, data)
tab2 <- table(Predicted = pred, Actual = data$type)
tab2
fourfoldplot(tab2)
tab2[2, 2] / (tab2[2, 1] + tab2[2, 2])
#Accuracy of 0.9937069
1 - sum(diag(tab2)) / sum(tab2)
#Misclassification of 0.5233%

#sigmoid
type_model <- svm(type ~ ., data = data, kernel = "sigmoid")
summary(type_model)
pred <- predict(type_model, data)
tab3 <- table(Predicted = pred, Actual = data$type)
tab3
fourfoldplot(tab3)
tab3[2, 2] / (tab3[2, 1] + tab3[2, 2])
#Accuracy of 0.9798658
1 - sum(diag(tab3)) / sum(tab3)
#Misclassification of 2.755%


#radial - best
type_model <- svm(type ~ ., data = data)
summary(type_model)
pred <- predict(type_model, data)
tab4 <- table(Predicted = pred, Actual = data$type)
tab4
fourfoldplot(tab4)
tab4[2, 2] / (tab4[2, 1] + tab4[2, 2])
#Accuracy of 0.9969444
1 - sum(diag(tab4)) / sum(tab4)
#Misclassification of 0.2924%
#Best

#tuned model
set.seed(123)
trained_model <-
  tune(svm,
       type ~ .,
       data = data,
       ranges = list(epsilon = seq(0, 1, 0.1), cost = 2 ^ (2:9)))
#plot(trained_model)
summary(trained_model)
best_model <- trained_model$best.model
summary(best_model)

pred <- predict(best_model, data)
tab5 <- table(Predicted = pred, Actual = data$type)
tab5
fourfoldplot(tab5)
tab5[2, 2] / (tab5[2, 1] + tab5[2, 2])
#Accuracy of 0.9977583
1 - sum(diag(tab5)) / sum(tab5)
#Misclassification of 0.2000% Better
#Chose Radial kernel with a cost of 8

# Predict values and make confusion table for the test set
pred <- predict(best_model, newdata = test)
table(true = test$type, pred)
tab6 <- table(true = test$type, pred)
1 - sum(diag(tab6)) / sum(tab6)
#Misclassification of 0.2051% on test set
#calculate accuracy
tab6[2, 2] / (tab6[2, 1] + tab6[2, 2])
#Accuracy of 0.9993188 on test set
fourfoldplot(tab6)

par(mfrow = c(2, 3))
fourfoldplot(tab1)
fourfoldplot(tab2)
fourfoldplot(tab3)
fourfoldplot(tab4)
fourfoldplot(tab5)
fourfoldplot(tab6)






################Clustering

library(psych)
# Use k = 2, 3 & 4

wine <- read.csv("winequality.csv", header = TRUE)

# Take away ID
wine <- wine[,-1]

# Sets seed
set.seed(123)

# Apply K means to data, with 2, 3, & 4 clusters
model_k2 <- kmeans(wine, 2, nstart = 50)
model_k3 <- kmeans(wine, 3, nstart = 50)
model_k4 <- kmeans(wine, 4, nstart = 50)

# Attached the resulting cluster to the file
k2data <- cbind(wine, model_k2$cluster)
k3data <- cbind(wine, model_k3$cluster)
k4data <- cbind(wine, model_k4$cluster)

# Run descriptives
describeBy(wine, model_k2$cluster, mat = T)
describeBy(wine, model_k3$cluster, mat = T)
describeBy(wine, model_k4$cluster, mat = T)

fviz_cluster(model_k2, data = wine)
fviz_cluster(model_k3, data = wine)
fviz_cluster(model_k4, data = wine)


################ HIERARCHICAL CLUSTERING

wine_clust <- read.csv("winequality.csv", header = TRUE)

# Take away ID
wine_clust <- wine[,-1]

# Sets seed
set.seed(123)

hc.complete <- hclust(dist(wine_clust), method = "complete")
hc.average <- hclust(dist(wine_clust), method = "average")
hc.single <- hclust(dist(wine_clust), method = "single")

# Indicates the plot will have one row of charts and three columns
par(mfrow = c(1, 3))
plot(
  hc.complete,
  main = "Complete Linkage",
  xlab = "",
  sub = "",
  cex = .9
)
plot(
  hc.average,
  main = "Average Linkage",
  xlab = "",
  sub = "",
  cex = .9
)
plot(
  hc.single,
  main = "Single Linkage",
  xlab = "",
  sub = "",
  cex = .9
)



# Cut the tree at the level of 2,3 & 4 clusters
k2clusttree <- cutree(hc.complete, k = 2)
k3clusttree <- cutree(hc.complete, k = 3)
k4clusttree <- cutree(hc.complete, k = 4)

plot(k2clusttree)
plot(k3clusttree)
plot(k4clusttree)





wine_clust <-
  data.frame(wine_clust, k2clusttree, k3clusttree, k4clusttree)



describeBy(wine_clust, k2clusttree, mat = T)
describeBy(wine_clust, k3clusttree, mat = T)
describeBy(wine_clust, k4clusttree, mat = T)




###########               SNA




install.packages("network")
library(network)
install.packages("sna")
library(sna)
install.packages("igraph")
library(igraph)
install.packages("statnet.common")
library(statnet.common)


SNA <- read.csv(file = "snaSP19.csv", header = TRUE)

# create network
network1 <- network(SNA, directed = FALSE)

print.network(network1)

# create object for plotting
network2 <- graph_from_data_frame(SNA)

# metrics
centralization.degree(network2)
#Centralization of 0.3515625
network.density(network1)
#Network Density of 0.8562092

# plot
node.size = c(10, 10, 10)
plot.network(
  network1,
  attrname = NULL,
  label = network.vertex.names(network1),
  edge.label = TRUE,
  vertex.size = node.size * 0.25
)

#My ego network
Hunter_net <- network(ego.extract(network2)$Hunter, directed = FALSE)
plot.network(
  Hunter_net,
  attrname = NULL,
  label = network.vertex.names(Hunter_net),
  edge.label = TRUE
)


###########Tree-based methods

#Regression trees are random forests on continuous variables (random forest on residual sugar)

#Random Forest
wine_rf <- read.csv(file = "winequality.csv", header = TRUE, sep = ",")
#Extracts ID fields
wine_rf <- wine_rf[, -1]
# Sets seed
set.seed(123)


library(tree)
library(randomForest) # Random Forests
library(mlbench) # for the dataset
library(gbm) # Boosted trees
library(Metrics)
library(caret)


split <- sample(1:nrow(wine_rf), size = 0.7 * nrow(wine_rf))
train_sugar <- wine_rf[split, ]
test_sugar <- wine_rf[-split, ]



for (i in 1:12) {
  bag.wine = randomForest(residual.sugar ~ .,
                          mtry = i,
                          train_sugar,
                          importance = TRUE)
}
###Boosting

boost_1 <-
  gbm(
    residual.sugar ~ .,
    data = train_sugar,
    distribution = "gaussian",
    n.trees = 1300
  ) #Gradient boosted trees, dist=bernoulie for classification
pred.boost <- predict.gbm(boost_1, newdata = test_sugar, n.trees = 1300)
pred.boost[pred.boost < 0] <- 0
error.boost <- rmsle(pred.boost, test_sugar$residual.sugar)
error.boost

boost_2 <-
  gbm(
    residual.sugar ~ .,
    data = train_sugar,
    distribution = "tdist",
    n.trees = 1300
  ) #Gradient boosted trees, dist=bernoulie for classification
pred.boost2 <- predict.gbm(boost_1, newdata = test_sugar, n.trees = 1300)
pred.boost2[pred.boost < 0] <- 0
error.boost2 <- rmsle(pred.boost, test_sugar$residual.sugar)
error.boost2




# Transforms outcome into factor, necessary for classification
# or else it will assume it is a regression tree
data_factor = ifelse(wine_rf$type == 1, "White", "Red")
# appends factor to data
factored_data <- data.frame(wine_rf, data_factor)
factored_data$type <- NULL


# Builds a tree with entire dataset
treedata = tree(data_factor ~ ., factored_data)
summary(treedata)
# plots
plot(treedata)
text(treedata, pretty = 0)


index <- sample(1:nrow(factored_data), size = 0.7 * nrow(factored_data))
train_color <- factored_data[index, ]
test_color <- factored_data[-index, ]



# get the tree based on training set
traintree = tree(data_factor ~ ., train_color)

# Use it to predict test set
predtree = predict(traintree, test_color, type = "class")

# Confusion table and accuracy
conf <- table(predtree, test_color$data_factor)
fourfoldplot(conf)
Accuracy <- (conf[1, 1] + conf[2, 2]) / sum(conf)
false_positive <- conf[1, 2] / (conf[1, 1] + conf[1, 2])
1 - sum(diag(conf)) / sum(conf)

Accuracy
false_positive

#Misclassification of 2.25%




############Text Mining
install.packages("glue")
install.packages("SentimentAnalysis")
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(readr)
library(plyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(magrittr)
library(SentimentAnalysis)


# Read text files into data
franken <- read_lines("Frankenstein.txt")
# making a tibble for easier extraction
franken_tibble <- tibble(line = seq_along(franken), text = franken)

# inspect the tibble
head(franken_tibble)

# we will use the pipe operator %>% to pipe the tibble into
# unnest_tokens in order to extract all different words appearing in the file
# and store them in tidy_text. This is called tokenization
# we can tokenize by word, character, sentence, n-gram, result of a regex
franken_tidy <- franken_tibble %>%
  unnest_tokens(word, text)

# Inspect the result
franken_tidy

# Now we count occurences of words and sort them by descending frequency

#This is where I hit the error. Whether I included the sort, or not. This is how I've always done this function.
countwords <- count(franken_tidy, vars = word)

# Let's see
countwords

# remove stopwords
count_stop <- countwords %>%
  anti_join(stop_words)

# Let's look
head(count_stop, 10)

#
# What the "joy" words?
#
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
#------------------------------------
joy_words <- count_stop %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)
joy_words
#--------------------------------------

#
# Anger
#
nrcanger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

anger_words <- count_stop %>%
  inner_join(nrcanger) %>%
  count(word, sort = TRUE)
anger_words

############Sentiments using bing


bing <- get_sentiments("bing")

head(bing)

franken_sentiment <- franken_tidy %>%
  inner_join(bing)

franken_2 <-
  count(franken_sentiment,  index = line %/% 300, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%   # split into positive and negative
  mutate(sentiment = positive - negative) # build a new column with result

#Now we can plot these sentiment scores across the plot trajectory
plot(franken_2$index, franken_2$sentiment, type = "h")

Positive <- franken_2$sentiment >= 0
ggplot(franken_2, aes(x = index, y = sentiment, fill = Positive)) +
  geom_bar(stat = "identity",
           position = "identity") + scale_fill_manual(values = c("red", "steelblue"))

library(wordcloud)

count_stop %>%
  with(wordcloud(word, n, max.words = 100))





############Neural Networks
install.packages("devtools")
library(devtools)
devtools::install_github("rstudio/tensorflow")
library("tensorflow")
install_tensorflow()

install.packages("keras")
library(keras)
install.packages("onehot")
library(onehot)




# Create onehot object with information for
# one-hot encoding
encoded_data_info <- onehot(data, stringsAsFactors = TRUE)
# get the encoded data
encoded_data <- (predict(encoded_data_info, data))
6497 / 2
# Create train and test sets
train_data <- data.matrix(encoded_data[1:3248, c(1:3, 5:13)])
train_targets <- encoded_data[1:3248, 4]
test_data <- data.matrix(encoded_data[3249:6497, c(1:3, 5:13)])
# Normalize
mean <- apply(train_data[, -c(12, 13)], 2, mean)
std <- apply(train_data[, -c(12, 13)], 2, sd)

mean <- apply(test_data[, -c(12, 13)], 2, mean)
std <- apply(train_data[, -c(12, 13)], 2, sd)


# Design model
model <- keras_model_sequential() %>%
  layer_dense(units = 10,
              activation = "relu",
              input_shape = c(12))    %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 3, activation = "relu")


# summary of the model
summary(model)

# compile model
model %>% compile(optimizer = 'adam', loss = "mean_squared_logarithmic_error")


# Train the model
model %>% fit(
  train_data,
  train_targets,
  batch_size = 128,
  epochs = 500,
  verbose = 1
)

# Prediction for the test set
y <- predict_proba(model, test_data)


### Neural Networks average (AvNNet)


# tunable parameters:
# size = number of units in the hidden layer;
# decay = weight decay of the neural network (regularization parameter);
# bag = a logical for bagging for each repeat.

fitControl <- trainControl(method = 'cv', number = 5)

fit_avNnet <- train(
  residual.sugar ~ .,
  data = train,
  method = 'avNNet',
  preProcess = 'range',
  trControl = fitControl,
  tuneGrid = expand.grid(
    .size = c(1, 5, 10),
    .decay = c(0, 0.001, 0.1),
    .bag = FALSE
  ),
  trace = FALSE,
  maxit = 1000,
  linout = T
)
predict_avNnet <- predict(fit_avNnet, newdata = test)



tab <- table(Predicted = predict_avNnet, Actual = test$residual.sugar)
tab


# Confusion table and accuracy

Accuracy <- (tab[1, 1] + tab[2, 2]) / sum(tab)
false_positive <- tab[1, 2] / (tab[1, 1] + tab[1, 2])

Accuracy
false_positive
1 - sum(diag(tab)) / sum(tab)
