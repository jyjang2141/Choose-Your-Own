#Download packages if needed
if (!require(caret)) install.packages('caret')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readr)) install.packages('readr')
if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')

#Load packages
library("caret")
library("tidyverse")
library("readr")
library("dplyr")
library("ggplot2")

#Import dataset
reviews <- read_csv("Data/sephora_website_dataset.csv")

#Remove unnecessary columns. Remaining columns should be predictors and rating/name only.
reviews <- reviews |> 
  subset(select = -c(id, details, how_to_use, ingredients, options, limited_time_offer, size, MarketingFlags, MarketingFlags_content, URL))

#Adjust each class of predictors
reviews <- reviews |>
  mutate (rating = as.factor(rating),
          brand = as.factor(brand),
          category = as.factor(category),
          online_only = as.factor(online_only),
          exclusive = as.factor(exclusive),
          limited_edition = as.factor(limited_edition))

#Number of rows and columns
num_Rows <- nrow(reviews)
num_Cols <- ncol(reviews)
#Number of brands
num_Brands <- n_distinct(reviews$brand)
#Number of categories
num_Categories <- n_distinct(reviews$category)

#Header of reviews
head(reviews)

#Number of NAs
sapply(reviews, function(x) which(is.na(x)))

#Number of ratings bar plot
reviews |> ggplot(aes(rating)) + geom_bar() +
  labs(title = "Number of Ratings")
#Number of loves bar plot
reviews |> ggplot(aes(rating, love)) + geom_col() + 
  labs(title = "Number of loves versus Ratings",
       x = "Ratings",
       y = "Number of Loves")

#Most loved product rating bar plot
reviews |> 
  arrange(desc(love)) |>
  slice(1:10) |>
  ggplot() +
  geom_col(aes(x = reorder(name, -love), y = love)) +
  geom_point(aes(x = reorder(name, -love), y = as.numeric(as.character(rating))*260000), color = "orange", size = 3.5) +
  scale_y_continuous(name = "Number of Loves", sec.axis = sec_axis( ~ ./260000, name = "Mean Rating")) +
  labs(title = "Top 10 Most Loved Products",
       x = "Product") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

#Price versus rating line plot
mean_price_rating <- reviews |>
  group_by(rating) |>
  summarise(mean_price = mean(price), mean_value_price = mean(value_price)) |>
  ungroup() |>
  pivot_longer(cols=c("mean_price", "mean_value_price"),
               names_to = "price_value_price",
               values_to = "price")

mean_price_rating |> ggplot(aes(price, as.numeric(as.character(rating)), color = price_value_price)) + 
  geom_line() +
  labs(title = "Rating vs Price",
       x = "Price",
       y = "Rating") +
  scale_color_manual(name = "Price or Value Price", 
                     values = c("red", "blue"))

#Most expensive product rating bar plot
reviews |> arrange(desc(price)) |>
  slice(1:10) |>
  ggplot() +
  geom_col(aes(x=reorder(name, -price), y = price)) +
  geom_point(aes(x=reorder(name, -price), y=as.numeric(as.character(rating))*110), color="orange", size=3.5) +
  scale_y_continuous(name = "Price ($)", sec.axis = sec_axis( ~ ./110, name = "Mean Rating")) +
  labs(title = "Top 10 Most Expensive Products",
       x = "Product") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

#Calculate means for each special offer
online_only_mean <- reviews |> 
  group_by(online_only) |>
  summarise(only_online = mean(as.integer(as.character(rating))))

exclusive_mean <- reviews |>
  group_by(exclusive) |>
  summarise(exclusive = mean(as.integer(as.character(rating))))

limited_edition_mean <- reviews |>
  group_by(limited_edition) |>
  summarise(limited_edition = mean(as.integer(as.character(rating))))

#Combine means into one data frame
special_offer <- 
  bind_cols(online_only_mean, exclusive_mean, limited_edition_mean) |>
  pivot_longer(cols = !"online_only", names_to = "SpecialOffer", values_to = "Yes")

#Special offers plot
special_offer |>
  ggplot(aes(SpecialOffer, Yes, fill = online_only)) +
  geom_col() +
  facet_wrap(~online_only) +
  labs(title = "Special Offers and Their Mean Rating",
       y = "Rating",
       fill = "Is there a special offer?") +
  scale_fill_discrete(labels = c("No", "Yes"))

#Brand rating bar graph
mean_ratings_brands <- reviews |>
  group_by(brand) |>
  summarise(mean_rating = mean(as.integer(as.character(rating))), num_rating = sum(number_of_reviews)) |>
  ungroup()

mean_ratings_brands |>
  ggplot() + 
  geom_col(aes(x = reorder(brand, mean_rating), y = mean_rating)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 1)) +
  labs(title = "Brands and their mean rating",
       x = "Brand",
       y = "Mean Rating")

#Brand rating bar graph (against number of reviews) - top 10
top_brands <- mean_ratings_brands |>
  arrange(desc(mean_rating)) |>
  slice(1:10)

top_brands |>
  ggplot() +
  geom_col(aes(x = reorder(brand, -mean_rating), y = mean_rating)) +
  geom_point(aes(x = reorder(brand, -mean_rating), y = num_rating/130), color = "orange", size = 3.5) +
  scale_y_continuous(name = "Mean Rating", sec.axis = sec_axis( ~ .*130, name = "Number of Reviews")) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10)) +
  labs(title = "Top 10 Brands with Highest Ratings",
       x = "Brand",
       y = "Mean Rating")

#Category rating bar graph (against number of reviws) - top 10
mean_ratings_category <- reviews |>
  group_by(category) |>
  summarise(mean_rating = mean(as.integer(as.character(rating))), num_rating = sum(number_of_reviews)) |>
  ungroup() |>
  arrange(desc(mean_rating)) |>
  slice(1:10)

mean_ratings_category |>
  ggplot() +
  geom_col(aes(x = reorder(category, -mean_rating), y = mean_rating)) +
  geom_point(aes(x = reorder(category, -mean_rating), y = num_rating/2800), color = "orange", size = 3.5) +
  scale_y_continuous(name = "Mean Rating", sec.axis = sec_axis( ~ .*2800, name = "Number of Reviews")) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10)) +
  labs(title = "Top 10 Categories with Highest Ratings",
       x = "Category",
       y = "Mean Rating")

#Separate the dataset into test and train sets
reviews <- reviews |> mutate(brand = str_replace(reviews$brand, "SEPHORA COLLECTION", "'Sephora Collection'"))

set.seed(3)
test_ind <- createDataPartition(reviews$rating, times = 1, p = 0.2, list = FALSE)
test_set <- reviews[test_ind,]
train_set <- reviews[-test_ind,]

#Remove rows that are unique to the training set
unmatched_brand <- anti_join(train_set, test_set, by = "brand")
unmatched_category <- anti_join(train_set, test_set, by = "category")

train_set <- train_set[-(which(train_set$brand %in% unmatched_brand$brand)),]
train_set <- train_set[-(which(train_set$category %in% unmatched_category$category)),]

#Remove rows that are unique to the test set
unmatched_brand_test <- anti_join(test_set, train_set, by = "brand")
unmatched_category_test <- anti_join(test_set, train_set, by = "category")

test_set <- test_set[-(which(test_set$brand %in% unmatched_brand_test$brand)),]
test_set <- test_set[-(which(test_set$category %in% unmatched_category_test$category)),]

#Set parameters for models
control <- trainControl(method = "cv", number = 10)

#Model 1 - KNN
model_knn <- train(rating ~ brand + category + number_of_reviews + love + price + value_price + online_only + exclusive + limited_edition, 
                   method = "knn", 
                   data = train_set,
                   trControl = control,
                   tuneLength = 10)

model_knn$resultsx
accuracy_knn <- confusionMatrix(predict(model_knn, test_set), test_set$rating)$overall["Accuracy"]
accuracy_knn
accuracy_list <- tibble(model = "KNN", accuracy = accuracy_knn)
accuracy_list

#Model 2 - Random Forest
#This model may take a few minutes to run.
model_rf <- train(rating ~ brand + category + number_of_reviews + love + price + value_price + online_only + exclusive + limited_edition, 
                  data = train_set,
                  trControl = control,
                  method = "rf")

model_rf$results
accuracy_rf <- confusionMatrix(predict(model_rf, test_set), test_set$rating)$overall["Accuracy"]
accuracy_rf
accuracy_list <- accuracy_list |> add_row(model = "RF", accuracy = accuracy_rf)
accuracy_list

#Model 3 - Support Vector Machine
#This model may take a few minutes to run.
model_svm <- train(rating ~ brand + category + number_of_reviews + love + price + value_price + online_only + exclusive + limited_edition, 
                  data = train_set,
                  trControl = control,
                  method = "svmRadial")

model_svm$results
accuracy_svm <- confusionMatrix(predict(model_svm, test_set), test_set$rating)$overall["Accuracy"]
accuracy_svm
accuracy_list <- accuracy_list |> add_row(model = "svm", accuracy = accuracy_svm)
accuracy_list
