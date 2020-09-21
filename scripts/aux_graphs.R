library('ggplot2')
library('readr')
library('dplyr')
library('tidyr')
library('zoo')
library('ggjoy')
library('stringr')

setwd('./Documents/ds/interviews/kavak/')
data <- read_csv(
  './data/raw/train.csv', 
  col_types=cols(
    last_review = col_date("%Y-%m-%d"),
    first_review = col_date("%Y-%m-%d"),
    host_since = col_date("%Y-%m-%d")
    )
  )

data <- data %>% mutate(
  host_since_month = as.yearmon(host_since),
  host_since_days = max(host_since,na.rm = TRUE) - host_since,
  host_response_rate = as.numeric(str_replace_all(host_response_rate,"%","")),
  has_thumbnail = ifelse(is.na(thumbnail_url), "0", "1"),
  accommodates_str = ordered(accommodates),
  bathrooms_str = ordered(bathrooms),
  host_response_rate_str = ordered(host_response_rate),
  score_rating = ordered(
    ifelse(
      review_scores_rating < 60, 
      '1', 
      ifelse(review_scores_rating < 80, '2', '3'))
    ),
  reviews = ifelse(number_of_reviews < 2, 'bad', 'good')
)


############## City
ggplot(data, aes(x=log_price, color=city)) + geom_density() + labs(title="Price per city")
ggplot(data, aes(x=city)) + geom_bar() + labs(title="# Observations per city")

# Hosts since
hosts_month <- data %>% group_by(host_since_month,city) %>% summarise(n=n())
ggplot(hosts_month, aes(x=host_since_month, y=n, color=city)) + geom_line()
# Conclusions: Overall price and characteristc differences between cities are small, we have more observations in NY
# than the rest of the cities and rentals in DC seem to be the "biggest" and in NY the smallest

############## Price
ggplot(data, aes(x=log_price, y=property_type)) + geom_joy()
ggplot(data, aes(x=log_price, y=room_type)) + geom_joy()
ggplot(data, aes(x=log_price, y=accommodates_str)) + geom_joy()
ggplot(data, aes(x=log_price, y=bathrooms_str)) + geom_joy()
ggplot(data, aes(x=log_price, y=bed_type)) + geom_joy()
ggplot(data, aes(x=log_price, y=cancellation_policy)) + geom_joy()
ggplot(data, aes(x=log_price, y=cleaning_fee)) + geom_joy()
ggplot(data, aes(x=log_price, y=host_has_profile_pic)) + geom_joy()
ggplot(data, aes(x=log_price, y=host_identity_verified)) + geom_joy()
ggplot(data, aes(x=log_price, y=host_response_rate)) + geom_jitter()
ggplot(data, aes(x=log_price, y=host_since_days)) + geom_jitter()
ggplot(data, aes(x=log_price, y=has_thumbnail)) + geom_joy()
ggplot(data, aes(x=log_price, y=number_of_reviews)) + geom_jitter()
ggplot(data, aes(x=log_price, y=reviews)) + geom_joy()
ggplot(data, aes(x=log_price, y=review_scores_rating)) + geom_jitter()
ggplot(data, aes(x=log_price, y=score_rating)) + geom_joy()
# Conclusion: Variables that seem to matter the most are room_type, accommodates, bathrooms, cleaning_fee and review_scores_rating

############## Others
ggplot(data, aes(x=bathrooms, y=accommodates_str)) + geom_joy()

############## Reviews
data$number_of_reviews_group <- floor(data$number_of_reviews/50)
avg_review <- data %>% group_by(number_of_reviews_group) %>% summarise(avg_price = median(log_price, na.rm = TRUE))
ggplot(avg_review, aes(x=number_of_reviews_group, y=avg_price)) + geom_line()

ggplot(data, aes(x=review_scores_rating)) + geom_bar()
avg_rating <- data %>% group_by(review_scores_rating) %>% summarise(avg_price = median(log_price, na.rm = TRUE))
ggplot(avg_rating, aes(x=review_scores_rating, y=avg_price)) + geom_line()
