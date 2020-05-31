#      * * * CAPSTONE PROJECT - MOVIELENS * * *      #  

# Author: Grifo Vincenzo


################################
# Create edx set, validation set
################################

# In this section the datasets will be sourced and undergo to some data-preprocessing.
# The retrieved dataset will be split into the working dataset *edx* and the validation dataset *validation*

# Note: this process could take a couple of minutes

# Loading the libraries that will be used
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



################################
# DATA EXPLORATION
################################

# In this section we will have look at some of the general properties of the dataset 
# to better understand the data and challenges ahead

# Analysing the structure of the dataset edx
str(edx)

# Having a look at the first lines of the dataset
head(edx)
# We notice:
# - title includes the year the movie was released within brakets
# - genres may include more than 1 genre separated by the special character |

# Checking if there are any NA/empty values
sum(is.na(edx))
# Sum is zero, therefore there are no na values



#  ----- USERS AND MOVIES FEATURES EXPLORATION ----- #

# Let's determine the number of unique users that have submitted a review and how many unique movies were rated
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))
# Not all the users have rated all the movies in the dataset

# Plotting the distribution of the users 
edx %>% 
  group_by(userId) %>% 
  summarize(n = n() ) %>% 
  ggplot(aes(x=n)) + 
  geom_histogram(binwidth = 25) + 
  xlim(0,1250) +
  ggtitle("Users Distribution") + 
  xlab("User number of ratings") +
  ylab("Frequency")
# -> We have limited the values displayed in the x-axis to make the chart more readable, since the distribution has a very long tail
# -> The warning message tells us how many values we're missing (same with the movies distribution below)

# and movies distribution
edx %>% 
  group_by(movieId) %>% 
  summarize(n = n() ) %>% 
  ggplot(aes(x=n)) + 
  geom_histogram(binwidth = 50) +
  xlim(0,5000) +
  ggtitle("Movies Distribution") + 
  xlab("Movie number of ratings") +
  ylab("Frequency")

# Computing the distribution of the average rating per movie:
edx %>% 
  group_by(movieId) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 30, color = "black") +
  ggtitle("Movie average rating distribution") +
  xlab("Movie average rating") +
  ylab("Frequency")

# -> We can notice  a significant variability of average rating across movies. 

# Computing the average rating for users u who have rated over 100 movies:
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black") +
  ggtitle("User average rating distribution") +
  xlab("User average rating") +
  ylab("Frequency")

# -> we can notice a substantial variability across users
# -> this suggests that the rating might be influenced by the user that provides such rating (user effect) 


#  ----- TIMESTAMP FEATURE EXPLORATION ----- #

# Plotting the average rating against the date to see if there is evidence of time effect on ratings
edx %>% mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = 'loess') +
  ggtitle("Average rating vs date of the rating") +
  xlab("Date of the rating") +
  ylab("Average Rating")

# -> We can see from the plot that there is somewhat a time effect, although it is not strong

# Having exact date of when the film was rated, we can actually engineer some new features that might actually have higher predictive power than the timestamp
# Specifically we can obtain:
# - a) time elapsed between the release of the movie and the rate given by the user
# - b) time elapsed between the review and the user's first review


#a) Plotting the average rating against the time elapsed between the release of the movie and the rate given by the user

edx %>% mutate(date = as_datetime(timestamp)) %>%
        # extract year when the movie was filmed 
        mutate(year_release = str_extract(title, "(?<=\\()\\d{4}+(?=\\))")) %>%
        # calculate time elapsed between review and movie's release
        mutate(years_from_release = as.numeric(year(date)-as.numeric(year_release))) %>%
        group_by(years_from_release) %>%
        summarize(rating = mean(rating)) %>%
        ggplot(aes(years_from_release, rating)) +
        geom_point() +
        geom_smooth(formula = y ~ x, method = 'loess') +
        ggtitle("Average rating vs year of release") +
        xlab("Number of years from the release of the movie") +
        ylab("Average Rating")

# Plotting the number of ratings vs the time elapsed between movie's release and review
edx %>% 
  mutate(date = as_datetime(timestamp),year_release = str_extract(title, "(?<=\\()\\d{4}+(?=\\))")) %>%
  mutate(years_from_release = as.numeric(year(date)-as.numeric(year_release))) %>%
  group_by(years_from_release) %>%
  summarize(n_rates = n()) %>%
  ggplot(aes(years_from_release, n_rates)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = 'loess') +
  ggtitle("Number of ratings vs years from movie's release") +
  xlab("Number of years from the movie's release") +
  ylab("Number of rating given")

# -> We can see that the majority of ratings happened for movie that have been released recently. 
# -> Just a limited number of movies get ratings 20+ years after their release. Most likely these are the classic movie that survive the judjment of time

# -> Let's confirm that the movies with most ratings and that have been rated after 25+ years from the realease are mostly classis
edx %>% mutate(date = as_datetime(timestamp), year_release = str_extract(title, "(?<=\\()\\d{4}+(?=\\))")) %>%
  mutate(years_from_release = as.numeric(year(date)-as.numeric(year_release))) %>%
  group_by(movieId, title,years_from_release) %>%
  summarize(n_rates = n()) %>%
  filter(years_from_release >25) %>%
  ungroup() %>%
  group_by(movieId, title) %>%
  arrange(desc(n_rates)) %>%
  head(20)  


# b) Time elapsed between the review and the user's first review

# Calculating the date of the first rating for each user and store this info in table 'first_rates'

# Function to calculate how many months have elapsed between 2 dates
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#Plotting the average rating vs number of years elapsend since the user's first rating
edx %>% 
  group_by(userId) %>% 
  mutate(date = as_datetime(timestamp), date_first_rate=min(date)) %>%
  ungroup() %>% 
  mutate(years_elapsed_first_review = ceiling(elapsed_months(date, date_first_rate)/12)) %>% 
  group_by(years_elapsed_first_review) %>% 
  summarise(avg_rate = mean(rating)) %>%
  ggplot(aes(years_elapsed_first_review, avg_rate )) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method = 'loess') +
  ggtitle("Average rating vs years elapsed from first rating") +
  xlab("Number of years elapsed since user's first rating") +
  ylab("Average rating")+
  xlim(0,10)

  
#  ----- GENRES FEATURE EXPLORATION ----- #

# Determining how many genres are included in the dataset (797)
length(unique(edx$genres))

# Let's see how many times a movie belonging to a specific genre receive a rating and plot this distribution
edx %>% 
  group_by(genres) %>% 
  summarize(n = n()) %>% 
  ungroup %>% 
  summarise(avg_n = mean(n), median = median(n), min = min(n), max=max(n))

edx %>% 
  group_by(genres) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x=n))+
  geom_histogram(bins = 100, color = "black") +  
  xlim(-1000, 50000) +
  ggtitle("Genres distribution") + 
  xlab("Number of ratings") +
  ylab("Frequency")

# Generating a bar plot of the average and standard error of each genre to determine whether there are evidence of a genre effect. 
edx %>% 
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  # Visualising only only genres with a minimum of 1000 reviews
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(size =4,angle = 90, hjust = 1)) +
  ggtitle("Average rating vs Genres") + 
  xlab("Genre") +
  ylab("Average rating")

# -> The x-axis labels are not very readable, because there are too many genres, however  we can still see from the shape of the curve some evidence of a genre effect. 



################################
# DATA CLEANING & PRE-PROCESSING
################################

# In this section the dataset edx will processed in order to include the new features engineered in the data exploration analysis
# and that will be going to be used sistematically during the creation and testing of the alghorithms.
# Furthermore, the section includes the creation of the training and test sets.

# --- Feature engineering ---
# 1. Creating a new column DATE with the date of when the rating was given from the column 'timestamp'.
edx <- edx %>% mutate(date = as_datetime(timestamp))

# 2. Creating a new column YEAR_RELEASE that contains the year when the movie was released
#   a. extract year when the movie was filmed
edx <- edx %>%
  mutate(year_release = str_extract(title, "(?<=\\()\\d{4}+(?=\\))")) %>%
# b calculate 'years from release' 
  mutate(years_from_release = as.numeric(year(date)-as.numeric(year_release))) 

# 3. Calculating the date of the first rate for each users (DATE_FIRST_RATE) and storing this info in table 'first_rates'
first_rates <- edx %>% 
  group_by(userId) %>% 
  summarise(date_first_rate=min(date), year_first_rate=(year(date_first_rate)))


# --- Splitting the edx dataset into TEST and TRAINING set ---

set.seed(17, sample.kind="Rounding")

# The test set will be 5% of the training set
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.05, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# Removing the temporary tables
rm(test_index, temp, removed)



################################
# BUILDING THE MODEL
################################

# In this section all the machine learning models will be built and the relative RMSEs will be calculated 


# Defining the Loss Function (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#   -----------  MODEL N 1. - BASELINE PREDICTION AVERAGE OF ALL RATINGS  ----------- 
#Baseline prediction by assigning the same rating to all movies. The rating will be the average of all ratings

# Average of all ratings
mu_hat <- mean(train_set$rating)

# Calculating RMSE
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

# Building a table where we are going to store the RMSEs of all the models that we will be created to facilitate a comparison between models
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results


#   -----------  MODEL N 2. - Including MOVIE EFFECT -----------
# In this model the predictions will be made by summing the least square estimates b_i (movie effects) to the average of all ratings

# Calculating the least square estimates b_i
mu <- mean(train_set$rating) #Average of all ratings

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Plotting the distribution of the estimates b_i
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"), main = "Distribution of b_i", xlab= "b_i" , ylab= "Frequency")

# Making predictions
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

# RMSE calculation
movie_effect_rmse <- RMSE(predicted_ratings, test_set$rating)

# Adding this RMSE to the table where we are storing all RMSE previously calculated
rmse_results <- tibble(method = c("Just the average","Movie Effect"), RMSE = c(naive_rmse,movie_effect_rmse))
rmse_results
# -> We can see that the addition of the 'movie effect' has improved the RMSE by 0.117


#   ------ THIRD MODEL MODEL - Including USER EFFECT ------
# In this model the predictions will be made by summing the the least square estimates b_u (user effects) to the average of all ratings and the movie effects b_i

# -> To fit a model that includes the user effect, we could use the code below. 
#    fit <- lm(rating ~ as.factor(movieId) + as.factor(userId), data= train_set)
#    However, as this cold would not be exectutable in most of the average laptops, we can compile a code that computes an estimate of the user-specif effects b_u

# Estimating b_u 
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Calculating predictions
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculating RMSE
user_effect_rmse <- RMSE(predicted_ratings, test_set$rating)

# Adding this RMSE to the table where we are storing all RMSE previously calculated
rmse_results <- tibble(method = c("Just the average","Avg + Movie Effect", "Avg + Movie Eff + User Eff"), RMSE = c(naive_rmse,movie_effect_rmse, user_effect_rmse))
rmse_results
#  -> we can see that we the addition of the 'user effect' the RMSE has improved by 0.079


#   ------ FOURTH MODEL MODEL - Including REGULARISATION ------
# In this section the process of 'regularisation' will be applied on the movie and user effects 
# to constrain the total variability of the effect sizes by penalizing large estimates that come from small sample sizes

# Before applying the regularisation on the movie effects let's first understand 
# if the performance of our current models has been affected by large estimates that comes from small sample sizes

# Visualising the worst 100 predictions 
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:100) %>% 
  pull(title)

# -> we notice that if we exclude the movies that appear multiple times (which are extremely famous), the movie that appear only once are quite unknown and niche. 
# -> maybe these movies have not been rated many many times? and could affect the performance of the model? 

# Let’s verify this assumptionby identifing the top 10 best and worst movies according to the predictions

# 1. Create a dataset with all the movie titles
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

# 2. Worst 10 movies according to the predictions
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)

# 3. Best 10 movies according to the predictions
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

# -> what we notice is that both the worst and best 10 movies are not that famous

# 4. Calculating how many times the top 10 best and worst movies have been rated to confirm the assumptions that these are niche movies

# 4a) Worst 10 movies number of ratings
train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)
#  --> Most of those movies have been rated just few times

# 4b) Best 10 movies number of ratings
train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)
#  --> All of those movies have been rated less than 5 times

# -> This that to improve the prediction then these large estimates that come from using small sample sizes should be penalise through the REGULARISATION process


# > ----- Implementing Regularisation -----

# Tuning the penalty term lambda through cross-validation of traning set

# 1) Create the index 'folds' that can be used to split the training set into  10 equally sized folds
set.seed(87, sample.kind = "Rounding")
folds <- createFolds(train_set$rating, k = 10, list = TRUE, returnTrain = FALSE)


# 2) Calculating the value of lambda that maximise the RMSE in each folds 

# Initialising the vector that will contain all the possible values of lambda that we will use during the tuning process
lambdas <- seq(0, 10, 0.20)

# Initialising the dataframe that will store the value of the RMSE for each value of lambda obtained in each folds
rmse_folds <- as.data.frame(matrix(ncol=length(lambdas), nrow=10))
colnames(rmse_folds) <- lambdas


# Creating a loop to implement the cross-validation process. 
# The loop will consist in 10 iteration (as we have 10 folds); in each iteration we will:
# 1) Use the combination of 9 of the 10 folds as a training set to train the model for each value of lambda, while using the remaining fold as a test set
# 2) For each value of lambda, training the model using the 'regularised" values of the least square estimates b_i and calculating the RMSE 
# -> at the end of the loop we will select the lambda with the minimum average RMSE

for (i in 1:10) {
  # Creating the train and test folds
  cv_train_set <- train_set[ -folds[[i]], ] 
  temp <-  train_set[ folds[[i]], ] 
  
  # Make sure userId and movieId in test set are also in train set
  cv_test_set <- temp %>% 
    semi_join(cv_train_set, by = "movieId") %>%
    semi_join(cv_train_set, by = "userId")
  
  # Add rows removed from test set back into train set
  removed <- anti_join(temp, cv_test_set)
  cv_train_set <- rbind(cv_train_set, removed)
  
  # Removing the temporary tables
  rm( temp, removed)
  
  # Training the model 
  mu <- mean(cv_train_set$rating)
  just_sum <- cv_train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
  
  # Calculating the regularised b_i for each value of lambda
  rmses <- sapply(lambdas, function(l){
  predicted_ratings <- cv_test_set %>% 
    left_join(just_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, cv_test_set$rating))
  })
  
  # Storing the RMSEs of each lambdas that have been calculated using the fold-i
  rmse_folds[i,] <- rmses
}
  
# Calculating the average RMSEs for each value of lambda 
 avg_rmse_folds <- colMeans(rmse_folds)
 
# Visualising avg_RMSE vs lambda
qplot(lambdas, avg_rmse_folds, main= "Average RMSE vs Lambda",xlab ="Lambda", ylab = "Average RMSE" )  

# Calculating and storing the value of lambda that minimise the avg RMSE (lambda = 2.2)
lambda <- lambdas[which.min(avg_rmse_folds)]
lambda

# Calculating the regularised estimates of b_i for the optimal lambda using the whole training set 
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 


#Now that we have the regularised estimate b_i we can analyse whether the performance of the model have improved:
# a) Movie effect vs Regularised Movie effect
# b) Regularised Movie effect + User effect


# a) Regularized Movie Effect model (no users-effect)
# Making the predictions 
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

# Calculating the RMSE of the regularised model
regularised_movie_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- tibble(method = c("Just the average","Avg + Movie Effect", "Avg + Movie Eff + User Eff", "Avg + Regul. Movie Eff"), RMSE = c(naive_rmse,movie_effect_rmse, user_effect_rmse,regularised_movie_effect_rmse))
rmse_results
# The penalised estimates provided a slight improvement over the least squares estimates of 0.0000232

# The RMSE has slightly improved, let's now check if best/worst movies according to this new model

# Top 10 best movies based on the penalized estimates b_i
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title)

# Worst 10 best movies based on the penalized estimates b_i
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(-b_i)) %>% 
  slice(1:10) %>% 
  pull(title)

# Let's confirm that now the top/worst movies have enough number of reviews

#  Best 10 movies number of ratings
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

#  Worst 10 movies number of ratings
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(-b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

#  --> We can notice that now both the best and worst movies have now more ratings than before


# b) Regularised movie Effect model (including users-effect)
# Making the predictions
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculating the RMSE of the new model
regularised_movie_and_user_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- tibble(method = c("Just the average","Avg + Movie Effect", "Avg + Movie Eff + User Eff", "Avg + Regul. Movie Eff","Avg + Regul. Movie Eff + User Eff"), RMSE = c(naive_rmse,movie_effect_rmse, user_effect_rmse,regularised_movie_effect_rmse,regularised_movie_and_user_effect_rmse))
rmse_results
# The penalised estimates plus the user effect provided a slight improvement of 0.00008 over the least squares estimates 



#   ------ FIFTH MODEL MODEL - Including the feature YEARS SINCE MOVIE'S RELEASE  ------
# In this model the effects of the feature engineered 'Years since movie's released' will be added to the latest model

# Estimating the least square estimates b_r and save these in the table release_avgs
release_avgs <- train_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  mutate(years_from_release = as.numeric(year(date)-as.numeric(year_release))) %>% 
  
  group_by(years_from_release) %>%
  summarize(b_r = mean(rating - mu - b_i - b_u))

# Plotting b_r
release_avgs %>% 
  ggplot(aes(x=years_from_release, y=b_r)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = 'loess', span=0.2) +
  ggtitle("b_r vs Years from release") +
  xlab("Years from movie's release") +
  ylab("b_r") 

# Veryfing the sample sizes for the largest values of 'Years from movie's release'
  train_set %>% 
  mutate(years_from_release = as.numeric(year(date)-as.numeric(year_release))) %>% 
  group_by(years_from_release) %>%
  summarize(n_obs = n()) %>%
  tail(20)
  # -> for values of 'years from movie's release'>86 the samples contain less than 200 observations. 

  # Plotting smooth function for b_r  with years_from_release<87 
  release_avgs %>% 
    filter(years_from_release<87) %>% 
    ggplot(aes(x=years_from_release, y=b_r)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = 'loess', span=0.2) +
    ggtitle("b_r vs Years from release (<87)") +
    xlab("Years from movie's release") +
    ylab("b_r")   
  # -> definitiely a better fit

  # Fitting the smooth function that can estimate every value of b_r for any given value of years_from_release
  fit <- release_avgs %>% 
  filter(years_from_release<87) %>% 
  # Selecting only 'years_from_release' with at least 200 observations
  loess(formula = b_r~years_from_release,control=loess.control(surface="direct"), span=0.2)
# As we have seen from the chart, the smooth function with span=0.2 gives us a good aproximation of the b_fr estimates

# Making predictions
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(release_avgs, by='years_from_release') %>%
  mutate(b_r = predict(fit, years_from_release), pred = mu + b_i + b_u + b_r)  %>%
  # The predict function is needed to estimate the value of b_fr for any given value of 'years_from_first_rate'
  pull(pred)

# Calculating RMSE
years_from_rel_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- tibble(method = c("Just the average","Avg + Movie Effect", "Avg + Movie Eff + User Eff", "Avg + Regul. Movie Eff","Avg + Regul. Movie Eff + User Eff", "Regul. Movie Eff + User Eff+Years from Rel eff"), RMSE = c(naive_rmse,movie_effect_rmse, user_effect_rmse,regularised_movie_effect_rmse,regularised_movie_and_user_effect_rmse,years_from_rel_effect_rmse))
rmse_results

# This new model has a lower RMSE by 0.000365


#   ------ SIXTH MODEL MODEL - Including the feature YEARS FROM USER'S FIRST RATING ------
# In this model the effects of the feature engineered 'Years since user's first rating' will be added to the latest model

# Function to calculate how many months have elapsed between 2 dates
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

# Estimating the least square  b_fr and saving these in the table years_from_first_rate_avg
years_from_first_rate_avg <- train_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(first_rates, by='userId') %>% 
  mutate(years_from_first_rate = ceiling(elapsed_months(date, date_first_rate)/12)) %>%
  # we are using the function created previously to calculate how many years have elapsed since the user's rating - we divide by 12 as there are 12 months in a year
  left_join(release_avgs, by='years_from_release') %>%
  group_by(years_from_first_rate) %>%
  summarize(b_fr = mean(rating - mu - b_i - b_u - b_r))  

# Plotting b_fr
years_from_first_rate_avg %>% 
  ggplot(aes(x=years_from_first_rate, y=b_fr)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = 'loess', span=0.5) +
  ggtitle("b_fr vs Years from first rate") +
  xlab("Years elapsed since user's first rating") +
  ylab("b_fr") 

# Fitting the smooth function that can estimate every value of b_gr for any given value of years from first rate
fit <- years_from_first_rate_avg %>%
  loess(formula = b_fr~years_from_first_rate,control=loess.control(surface="direct"), span=0.5)
# As we have seen from the chart, the smooth function with span =0.5 gives us a good aproximation of the b_fr estimates

# Making predictions
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(first_rates, by='userId') %>% 
  mutate(years_from_first_rate = ceiling(elapsed_months(date, date_first_rate)/12)) %>%
  left_join(release_avgs, by='years_from_release') %>%
  left_join(years_from_first_rate_avg, by='years_from_first_rate') %>%
  mutate(b_fr = predict(fit, years_from_first_rate), pred = mu + b_i + b_u + b_r+ b_fr)  %>%
  # The predict function is needed to estimate the value of b_fr for any given value of 'years_from_first_rate'
  pull(pred)

years_from_first_rate_effect_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- tibble(method = c("Just the average","Avg + Movie Effect", "Avg + Movie Eff + User Eff", "Avg + Regul. Movie Eff","Avg + Regul. Movie Eff + User Eff", "Regul. Movie Eff + User Eff+Years from Rel eff", "Avg. + Reg. Movie + User + Years from rel. + Years from first rate"), RMSE = c(naive_rmse,movie_effect_rmse, user_effect_rmse,regularised_movie_effect_rmse,regularised_movie_and_user_effect_rmse,years_from_rel_effect_rmse, years_from_first_rate_effect_rmse))
rmse_results
# The RMSE has improved by 0.0000927


#   ------ SIXTH MODEL MODEL - Including GENRE EFFECT ------
# In this model the least square estimates b_g (genre effects) will be added to the previous model


# Estimating the least square the least square estimates b_g and save these in the table genre_avg
genre_avgs <- train_set %>% 
  left_join(first_rates, by='userId') %>%
  mutate(years_from_first_rate = ceiling(elapsed_months(date, date_first_rate)/12)) %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  group_by(userId) %>%
  left_join(user_avgs, by='userId') %>%
  left_join(release_avgs, by='years_from_release') %>%
  left_join(years_from_first_rate_avg, by='years_from_first_rate') %>%
  summarize(b_g = mean(rating - mu - b_i - b_u - b_r- b_fr))

# Calculating predictions
predicted_ratings <- test_set %>% 
  left_join(first_rates, by='userId') %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='userId') %>%
  mutate(years_from_first_rate = ceiling(elapsed_months(date, date_first_rate)/12)) %>%
  left_join(release_avgs, by='years_from_release') %>%
  left_join(years_from_first_rate_avg, by='years_from_first_rate') %>%
  mutate(b_fr = predict(fit, years_from_first_rate), pred = mu + b_i + b_u + b_r+ b_fr +b_g) %>%
  pull(pred)

# Calculating RMSE  
genre_effect_rmse <- RMSE(predicted_ratings, test_set$rating) 
rmse_results <- tibble(method = c("Just the average","Avg + Movie Effect", "Avg + Movie Eff + User Eff", "Avg + Regul. Movie Eff","Avg + Regul. Movie Eff + User Eff", "Regul. Movie Eff + User Eff+Years from Rel eff", "Avg. + Reg. Movie + User + Years from rel. + sems. from first rate","Avg. + Reg. Movie + User + Years from rel. + sems. from first rate,Years from rel. + Years from first rate +genre"), RMSE = c(naive_rmse,movie_effect_rmse, user_effect_rmse,regularised_movie_effect_rmse,regularised_movie_and_user_effect_rmse,years_from_rel_effect_rmse, years_from_first_rate_effect_rmse, genre_effect_rmse))
rmse_results
# _> We have improved the RMSE once again. This means that this will be our final model because it has the lowest RMSE=0.8653368

  

###########################################################################################
# FINAL STEP - USING THE VALIDATION DATASTE TO EVALUATE THE RMSE OF THE FINAL MODEL 
###########################################################################################

# Before using the validation set to test make prediction with the final model and calculate the RMSE, 
# we will need to preprocess the dataset with the same operations undertaken with the edx dataset

# ------ Data preprocessing ------
validation <- validation %>% mutate(date = as_datetime(timestamp))

# 1. Extracting the year when the movie was released
validation <- validation %>% mutate(year_release = str_extract(title, "(?<=\\()\\d{4}+(?=\\))")) %>%
# 2. Calculating the new feature 'years from release' 
mutate(years_from_release = as.numeric(year(date)-as.numeric(year_release)))

# ------ Making predictions ------
predicted_ratings <- validation %>% 
  left_join(first_rates, by='userId') %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='userId') %>%
  mutate(years_from_first_rate = ceiling(elapsed_months(date, date_first_rate)/12)) %>%
  left_join(release_avgs, by='years_from_release') %>%
  left_join(years_from_first_rate_avg, by='years_from_first_rate') %>%
  mutate(b_fr = predict(fit, years_from_first_rate), pred=mu + b_i + b_u + b_r+ b_fr +b_g) %>%
  pull(pred)

# ----- Calculating RMSE ------
final_RMSE <- RMSE(predicted_ratings, validation$rating) 
final_RMSE

