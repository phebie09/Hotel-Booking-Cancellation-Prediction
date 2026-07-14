##################################################
# MODELING SPECIALIST 2
# Association Rule Mining
##################################################

library(arules)
library(arulesViz)
library(dplyr)

hotel <- read.csv("hotel_booking_cleaned.csv")

##################################################
# Convert Variables to Factors
##################################################

association_data <- hotel %>%
  select(hotel,
         meal,
         market_segment,
         distribution_channel,
         reserved_room_type,
         customer_type,
         deposit_type,
         is_canceled)

association_data[] <- lapply(association_data,
                             as.factor)

##################################################
# Convert to Transactions
##################################################

transactions <- as(association_data,
                   "transactions")

summary(transactions)

##################################################
# Run Apriori
##################################################

rules <- apriori(
  transactions,
  parameter = list(
    supp = 0.02,
    conf = 0.70,
    minlen = 2
  )
)

##################################################
# Sort Rules
##################################################

rules_sorted <- sort(rules,
                     by = "lift")

inspect(head(rules_sorted,10))

##################################################
# Plot Rules
##################################################

plot(rules_sorted,
     method = "graph")

plot(rules_sorted,
     method = "grouped")

plot(head(rules_sorted,20),
     method = "paracoord")

##################################################
# Save Rules
##################################################

write(rules_sorted,
      file = "association_rules.csv",
      sep = ",")