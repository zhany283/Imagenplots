



















library(umap)
library(ggplot2)
library(dplyr)

# Select only numeric columns for UMAP
numeric_data <- select_if(data, is.numeric)

# Remove "na"
numeric_data_clean <- na.omit(numeric_data)


umap_result <- umap::umap(numeric_data_clean)

umap_df$`Sample.ID` <- numeric_data_clean$`Sample.ID`



umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")

umap_plot <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2)) +
  geom_point(alpha = 0.5) + 
  geom_text(aes(label = `Sample.ID`), vjust = "inward", hjust = "inward", size = 2.5, check_overlap = TRUE) + # Add labels
  theme_minimal() +
  labs(title = "UMAP Projection with Sample IDs", x = "UMAP Dimension 1", y = "UMAP Dimension 2")

umap_plot









library(umap)
library(ggplot2)
library(dplyr)

numeric_data <- select_if(data, is.numeric)

# Remove NA
numeric_data_clean <- na.omit(numeric_data)

# UMAP
umap_result <- umap::umap(numeric_data_clean)

# Create a dataframe from UMAP results
umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")


data_clean <- data[!is.na(rowSums(select_if(data, is.numeric))),]

umap_df$`Sample.ID` <- data_clean$`Sample.ID` 

# Generate the UMAP plot with labels
umap_plot <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2)) +
  geom_point(alpha = 0.5) + 
  geom_text(aes(label = `Sample.ID`), vjust = "inward", hjust = "inward", size = 2.5, check_overlap = TRUE) +
  theme_minimal() +
  labs(title = "UMAP Projection with Sample IDs", x = "UMAP Dimension 1", y = "UMAP Dimension 2")

umap_plot

library(stats)

# Perform k-means clustering on the UMAP results
set.seed(123) # For reproducibility
k <- 5
kmeans_result <- kmeans(umap_df[, c("UMAP1", "UMAP2")], centers = k)

# Add the cluster assignments to umap_df
umap_df$cluster <- as.factor(kmeans_result$cluster)

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  geom_text(aes(label = `Sample.ID`), vjust = "inward", hjust = "inward", size = 2.5, check_overlap = TRUE) +
  labs(title = "UMAP Projections with k-means Clusters", x = "UMAP Dimension 1", y = "UMAP Dimension 2", color = "Cluster") +
  scale_color_brewer(palette = "Set1") 
