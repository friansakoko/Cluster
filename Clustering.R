#2 Features K-Means Clustering----
# Load the dataset
library(ISLR)
data(Credit)

# Select two features
selected_features <- Credit[, c("Age", "Rating")]
scaled_selected <- scale(selected_features)

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(scaled_selected, centers = 5, nstart = 25)

# Add cluster assignments to the data
Credit$Cluster <- kmeans_result$cluster
View(Credit)

# 2D Scatter plot of clusters
library(ggplot2)
ggplot(Credit, aes(x = Age, y = Rating, color = factor(Cluster))) +
  geom_point(size = 2) +
  labs(title = "K-means Clustering on Selected Features",
       x = "Age",
       y = "Rating",
       color = "Cluster") +
  theme_minimal()


#Find optimum K - Elbow Method ----
#Choose the low point
wcss <- sapply(1:10, function(k) {
  kmeans(scaled_selected, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wcss, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method")

#Find Optimum K - Silhouette Method ----
#Choose the high point
library(cluster)
library(dplyr)  # or library(magrittr)


sil_width <- sapply(2:10, function(k) {
  kmeans_result <- kmeans(scaled_selected, centers = k, nstart = 25)
  silhouette(kmeans_result$cluster, dist(scaled_selected))[, 3] %>% mean()
})

plot(2:10, sil_width, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Width",
     main = "Silhouette Method")


#Find Optimum K - Gap Statistic ----
#Choose the high point

library(cluster)
set.seed(123)
gap_stat <- clusGap(scaled_selected, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 50)
print(gap_stat)
plot(gap_stat)


#Find Optimum K - Davies-Bouldin Index ----
#Choose the low point
#install.packages("clusterSim")
library(clusterSim)

db_values <- sapply(2:10, function(k) {
  kmeans_result <- kmeans(scaled_selected, centers = k, nstart = 25)
  index.DB(scaled_selected, kmeans_result$cluster)$DB
})

plot(2:10, db_values, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Davies-Bouldin Index",
     main = "DB Index Method")


#3 Features K-Means Clustering----
library(ISLR)

# Load the dataset
data(Credit)

# Select two features
selected_features <- Credit[, c("Age", "Rating", "Education")]
scaled_selected <- scale(selected_features)

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(scaled_selected, centers = 3, nstart = 25)

# Add cluster assignments to the data
Credit$Cluster <- kmeans_result$cluster
View(Credit)

# 3D Scatter plot of clusters
library(plotly)
#install.packages("plotly")

plot_ly(Credit, x = ~Age, y = ~Rating, z = ~Education, 
        color = ~factor(Cluster), type = 'scatter3d', mode = 'markers')


#2 Features DB-SCAN CLustering----
# Load necessary libraries
#install.packages("dbscan")
library(ISLR)
library(dbscan)

# Step 1: Load and prepare the data
data(Credit)
selected_features <- Credit[, c("Age", "Rating")]

# Step 2: Standardize the features (recommended for DBSCAN)
scaled_features <- scale(selected_features)

# Step 3: Apply DBSCAN
# Set eps (neighborhood size) and minPts (minimum points for a cluster)
dbscan_result <- dbscan(scaled_features, eps = 0.4, minPts = 7)

# Step 4: Inspect the clustering result
print(dbscan_result)

# Add cluster labels back to the original dataset
Credit$Cluster <- dbscan_result$cluster

# View the updated dataset
View(Credit)

# Step 5: Visualize the clusters
library(ggplot2)
ggplot(Credit, aes(x = Age, y = Rating, color = factor(Cluster))) +
  geom_point() +
  theme_minimal() +
  labs(title = "DBSCAN Clustering of Credit Data",
       color = "Cluster") +
  theme(legend.position = "bottom")

#3 Features DB-SCAN Clustering----
# Load necessary libraries
#install.packages("dbscan")
library(ISLR)
library(dbscan)

# Step 1: Load and prepare the data
data(Credit)
selected_features <- Credit[, c("Age", "Rating", "Education")]

# Step 2: Standardize the features (recommended for DBSCAN)
scaled_features <- scale(selected_features)

# Step 3: Apply DBSCAN
# Set eps (neighborhood size) and minPts (minimum points for a cluster)
dbscan_result <- dbscan(scaled_features, eps = 0.8, minPts = 6)

# Step 4: Inspect the clustering result
print(dbscan_result)

# Add cluster labels back to the original dataset
Credit$Cluster <- dbscan_result$cluster

# View the updated dataset
View(Credit)

# Step 5: Visualize the clusters
# 3D Scatter plot of clusters
library(plotly)
#install.packages("plotly")

plot_ly(Credit, x = ~Age, y = ~Rating, z = ~Education, 
        color = ~factor(Cluster), type = 'scatter3d', mode = 'markers')

#Hierarchical Clustering----
data(swiss)
View(swiss)
hc <- hclust(dist(scale(swiss)), method = "average") 

# Plot the dendrogram
plot(hc, main = "Dendrogram for Swiss Data")

# Highlight the clusters (e.g., 3 clusters)
rect.hclust(hc, k = 6, border = "red")

# Assign clusters using cutree
clusters <- cutree(hc, k = 6)

# Add the cluster assignments as a new column 
swiss$Cluster <- clusters

# View the updated dataset
View(swiss)

