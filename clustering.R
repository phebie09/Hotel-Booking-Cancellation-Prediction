##################################################
# MODELING SPECIALIST 2
# Clustering Models
# k-Means and DBSCAN
##################################################

# Load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(dbscan)

set.seed(2026)

# Load cleaned dataset
hotel <- read.csv("hotel_booking_cleaned.csv")

##################################################
# Select numeric variables
##################################################

cluster_data <- hotel %>%
  select(where(is.numeric))

# Remove missing values
cluster_data <- na.omit(cluster_data)

##################################################
# Scale the data
##################################################

cluster_scaled <- scale(cluster_data)

##################################################
# Determine Optimal Number of Clusters
##################################################

fviz_nbclust(cluster_scaled,
             kmeans,
             method = "wss")

fviz_nbclust(cluster_scaled,
             kmeans,
             method = "silhouette")

##################################################
# k-Means Clustering
##################################################

kmeans_model <- kmeans(cluster_scaled,
                       centers = 3,
                       nstart = 25)

cluster_data$Cluster <- as.factor(kmeans_model$cluster)

##################################################
# Visualize Clusters
##################################################

fviz_cluster(kmeans_model,
             data = cluster_scaled,
             ellipse.type = "convex",
             ggtheme = theme_minimal())

##################################################
# Cluster Summary
##################################################

cluster_summary <- cluster_data %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean))

print(cluster_summary)

##################################################
# DBSCAN
##################################################

dbscan_model <- dbscan(cluster_scaled,
                       eps = 2,
                       minPts = 10)

fviz_cluster(dbscan_model,
             cluster_scaled,
             geom = "point")

table(dbscan_model$cluster)

##################################################
# Save Output
##################################################

write.csv(cluster_summary,
          "cluster_summary.csv",
          row.names = FALSE)