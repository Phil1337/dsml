#### https://rafalab.github.io/dsbook/recommendation-systems.html

# Create Train/Test sets
library(caret)
library(dplyr)
library(knitr)
set.seed(755)

train_set <- edx
test_set <- validation

# To make sure we don’t include users and movies in the test set that do not appear in the training set, we remove these entries using the semi_join function:
  
  test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Final Submission

# calculate simple model
mu <- mean(train_set$rating) 

### calculate movie average
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Predict using movie effects model
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i


### additionally calculate user average
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


### finally calculate genre average
genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u))


## Predict and create submission file
predicted_final <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(rating = mu + b_i + b_u + b_g) %>%
  .$rating

# Create submission file
submission <- test_set %>% 
  cbind(rating = predicted_final)

## Export submission file
write.csv(submission %>% select(userId, movieId, rating),
          "submission.csv", na = "", row.names=FALSE)

