library(tidyverse)
library(caret)
library(dplyr)
library(stringr)

hotel <- read.csv("hotel_booking.csv", stringsAsFactors = FALSE)


View(hotel)
str(hotel)
summary(hotel)


colSums(is.na(hotel))
round(colMeans(is.na(hotel))*100,2)


hotel <- hotel %>%
  distinct()

nrow(hotel)


hotel$children[is.na(hotel$children)] <- 0
hotel$country[is.na(hotel$country)] <- "Unknown"
hotel$agent[is.na(hotel$agent)] <- 0
hotel$company <- NULL


hotel <- hotel %>%
  select(-reservation_status,
         -reservation_status_date,
         -assigned_room_type)


hotel <- hotel %>%
  select(-name,
         -email,
         -phone.number,
         -credit_card)


hotel$hotel <- as.factor(hotel$hotel)

hotel$meal <- as.factor(hotel$meal)

hotel$market_segment <- as.factor(hotel$market_segment)

hotel$distribution_channel <- as.factor(hotel$distribution_channel)

hotel$deposit_type <- as.factor(hotel$deposit_type)

hotel$customer_type <- as.factor(hotel$customer_type)

hotel$is_canceled <- as.factor(hotel$is_canceled)


hotel <- hotel %>%
  filter(adults + children + babies > 0)

hotel <- hotel %>%
  filter(adr >= 0)


hotel$total_guests <-
  hotel$adults +
  hotel$children +
  hotel$babies

hotel$total_nights <-
  hotel$stays_in_week_nights +
  hotel$stays_in_weekend_nights


str(hotel)

summary(hotel)

View(hotel)


write.csv(hotel,
          "hotel_booking_cleaned.csv",
          row.names = FALSE)