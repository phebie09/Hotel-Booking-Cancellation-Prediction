##################################################
# MODEL EVALUATION
##################################################

library(cluster)
library(factoextra)
library(kableExtra)

##################################################
# Silhouette Score
##################################################

sil <- silhouette(kmeans_model$cluster,
                  dist(cluster_scaled))

mean(sil[,3])

fviz_silhouette(sil)

##################################################
# Cluster Sizes
##################################################

table(kmeans_model$cluster)

##################################################
# DBSCAN Cluster Sizes
##################################################

table(dbscan_model$cluster)

##################################################
# Comparison Table
##################################################

comparison <- data.frame(
  
  Model=c("k-Means",
          "DBSCAN",
          "Apriori"),
  
  Output=c(
    length(unique(kmeans_model$cluster)),
    length(unique(dbscan_model$cluster)),
    length(rules_sorted)
  )
  
)

comparison %>%
  kable(caption="Model Comparison") %>%
  kable_styling(full_width=FALSE)

write.csv(comparison,
          "model_comparison.csv",
          row.names=FALSE)