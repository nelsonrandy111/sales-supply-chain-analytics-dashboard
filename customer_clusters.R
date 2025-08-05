library(tidyverse)
library(cluster)
library(data.table)
library(tidyverse)
library(scales)
library(factoextra)
library(lattice)
library(tidymodels)
options(scipen = 999)
library(factoextra)


# loading data used for clustering
cluster <- read_csv('data/clustering_data.csv')
cluster_consumer <- cluster %>% 
  filter(IsConsumer==1)
cluster_consumer <- cluster_consumer %>% select(-starts_with("PctDepartment"))
cluster_consumer <- cluster_consumer %>% select(-all_of("IsConsumer"))




scale_features <- c("AvgShippingError", "Recency", "Tenure",
                    "TotalNumberOfOrders", "AverageDiscount",
                    "AverageOrderSales", "AverageOrderQuantity",
                    "AverageOrderProfit")

# scaling selected columns
cluster_scaled <- cluster_consumer %>% mutate(across(all_of(scale_features), scale))

# storing means and sd of scaled features
scaled_means <- map_dbl(cluster_scaled[scale_features], ~ attr(.x, "scaled:center"))
scaled_sds <- map_dbl(cluster_scaled[scale_features], ~ attr(.x, "scaled:scale"))

# storing means and sd of unscaled features
other_features <- setdiff(names(cluster_scaled), scale_features)

unscaled_means <- map_dbl(cluster_scaled[other_features], mean, na.rm = TRUE)
unscaled_sds <- map_dbl(cluster_scaled[other_features], sd, na.rm = TRUE)

# saving all means and sd together
all_means <- c(scaled_means, unscaled_means)
all_sds <- c(scaled_sds, unscaled_sds)

# # Save them to files
# saveRDS(all_means, "all_means.rds")
# saveRDS(all_sds, "all_sds.rds")



# PCA Components explaining about 80% variance
pca <- prcomp(cluster_scaled, scale. = FALSE)  # no scaling needed
summary(pca) 
# choosing 5 PCA dimensions
pca_data <- pca$x[, 1:5]

# saving PCA transformations to interpret clusters later
rotation_matrix <- pca$rotation[, 1:5]  # 5 principal components

# The K-means algorithm for clustering
km <- kmeans(x = pca_data, centers = 4, nstart = 15)

# Exploring the optimal number of clusters
kclusts <-
  tibble(k = 1:20) %>%
  mutate(
    kclust = map(k, ~ kmeans(pca_data, .x, nstart = 15)),
    metrics = map(kclust, ~ glance(.x))
  )


## Metrics:
# withinss: sum of squares within cluster (intra cluster variance)
# tot.withinss: TOTAL sum of squares within cluster
# betweenss: sum of squares between clusters (inter cluster variance)
# totss: total sum of squares (betweenss + tot.withinss)

kclusts$kclust[[4]]$withinss
kclusts$kclust[[4]]$betweenss
kclusts$metrics[[4]]
res <- unnest(data = kclusts, cols = c('k', 'metrics')) %>% 
  mutate(betweenss = round(betweenss, 3))

# plotting evaluations of iterative cluster numbers
ggplot(data = res %>% pivot_longer(cols = c(tot.withinss, betweenss), names_to = 'var', values_to = 'val'), aes(x = k, y = val, fill = var)) +
  # geom_line() +
  # geom_point() +
  geom_col() +
  scale_x_continuous(n.breaks = 20) +
  theme_light()

kclusts %>%
  unnest(cols = c(metrics)) %>%
  ggplot(aes(k, tot.withinss)) +
  geom_line(alpha = 0.5, linewidth = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue") +
  labs(title = 'TOTAL sum of squares within cluster') +
  theme_light()

kclusts %>%
  unnest(cols = c(metrics)) %>%
  mutate(var_expl = betweenss / totss) %>% 
  ggplot(aes(k, var_expl)) +
  geom_line(alpha = 0.5, linewidth = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue") +
  geom_vline(xintercept = 4, col = 'red', linewidth = 1) +
  labs(title = '% Variance explained') +
  scale_y_continuous(labels = percent) +
  theme_light()


# Calculate silhouette score
silhouette_scores <- tibble(k = 2:10) %>%
  mutate(
    km = map(k, ~ kmeans(pca_data, centers = .x, nstart = 15)),
    sil = map(km, ~ silhouette(.x$cluster, dist(pca_data))),
    avg_sil_width = map_dbl(sil, ~ mean(.x[, 3]))
  )

# Plot average silhouette score for cluster sizes
ggplot(silhouette_scores, aes(x = k, y = avg_sil_width)) +
  geom_line(color = "midnightblue", linewidth = 1.2, alpha = 0.6) +
  geom_point(size = 2, color = "midnightblue") +
  geom_vline(xintercept = silhouette_scores$k[which.max(silhouette_scores$avg_sil_width)], 
             linetype = "dashed", color = "red") +
  labs(title = "Average Silhouette Score for K-Means",
       x = "Number of clusters (k)",
       y = "Average silhouette width") +
  theme_light()

# Kmeans model with four clusters 
k <- 4
clustering <- kmeans(pca_data, centers = k, nstart = 15)
clusters <- as.factor(clustering$cluster)
summary(clusters) 
centroids <- clustering$centers 
centroids

# Reverse PCA
centroids_pca <- clustering$centers  # 4 x 5 matrix
centroids_scaled <- centroids_pca %*% t(rotation_matrix)
colnames(centroids_scaled) <- colnames(cluster_scaled)

# Reverse scaling
centroids_original <- centroids_scaled

# Unscale only scaled features using saved means/sds
centroids_original[, scale_features] <- sweep(
  centroids_original[, scale_features], 2, scaled_sds, "*"
)
centroids_original[, scale_features] <- sweep(
  centroids_original[, scale_features], 2, scaled_means, "+"
)
centroids_original


# Labeling cluster for each customer
cluster_consumer$cluster <- clusters

# summarising cluster data 
cluster_consumer_characteristics <- cluster_consumer %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
            n_customers = n()) %>% 
  mutate(cluster_name = case_when(
    cluster == 1 ~ "High-Value New",
    cluster == 2 ~ "Lapsed Frequent",
    cluster == 3 ~ "Mid-Value New",
    cluster == 4 ~ "Dormant Mid-Value")
  )


pca_df <- as.data.frame(pca_data[, 1:2])
pca_df$cluster <- as.factor(clustering$cluster)

pca_df <- pca_df %>%
  mutate(ClusterName = case_when(
    cluster == 1 ~ "High-Value New",
    cluster == 2 ~ "Lapsed Frequent",
    cluster == 3 ~ "Mid-Value New",
    cluster == 4 ~ "Dormant Mid-Value"
  ))

# Summarising centroids for two PCA dimensions
centers <- pca_df %>%
  group_by(cluster) %>%
  summarise(across(c(PC1, PC2), mean))


# plotting clusters using 2 PCA dimensions
ggplot(pca_df, aes(x = PC1, y = PC2, color = ClusterName)) +
  geom_jitter(alpha = 0.6, size = 1.2, width = 0.3, height = 0.3) + 
  stat_ellipse(type = "norm", level = 0.95, linetype = "solid", linewidth = 1) + 
  scale_color_brewer(
    palette = "Set2") +  
  labs(
    title = "K-Means Clusters in PCA Space (with 95% Confidence Ellipses)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  ) + geom_point(data = centers, aes(x = PC1, y = PC2), 
                 color = "black", shape = 4, size = 4, stroke = 1.2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

saveRDS(pca_df, file = 'customer_pca_df.rds')
saveRDS(cluster_consumer_characteristics, file = 'customer_cluster_summary.rds')
