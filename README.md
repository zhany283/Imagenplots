data <- read.csv("/Users/zhanyu/Downloads/Biorepository Summary - cell loss.csv")

# Check the frequencies of three sample processor
processed_by_counts <- table(data$`Processed.by`)
sorted_by_counts <- table(data$`Sorted.cryopreserved.by`)

processed_by_counts[c("Zhan", "Yuliana", "Estefani")]


sorted_by_frequencies <- sapply(c("Zhan", "Yuliana", "Estefani"), function(name) {
  sum(grepl(name, data$`Sorted.cryopreserved.by`))
})

processed_counts <- processed_by_counts[c("Zhan", "Yuliana", "Estefani")]
sorted_counts <- sorted_by_frequencies
processed_counts_vector <- as.numeric(processed_counts)
names(processed_counts_vector) <- names(processed_counts)

total_counts <- processed_counts_vector + sorted_counts

print(total_counts)



data$cellloss_numeric <- as.numeric(gsub("%", "", data$cellloss))
median_cellloss <- median(data$cellloss_numeric, na.rm = TRUE)
print(median_cellloss)

library(ggplot2)
library(dplyr)


ggplot(data, aes(x = `Sample.ID`, y = cellloss_numeric, color = `Sorted.cryopreserved.by`)) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "Dot Plot of Cell Loss by Sample ID",
       x = "Sample ID", y = "Cell Loss (%)") +
  ylim(40,100) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


ggplot(data, aes(x = `Sample.ID`, y = cellloss_numeric, color = `Sorted.cryopreserved.by`)) +
  geom_point(size = 2.5, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Dot Plot of Cell Loss by Sample ID",
       x = "Sample ID", y = "Cell Loss (%)") +
  ylim(40,100) +
  geom_text(aes(label = `Viability`), vjust = "inward", hjust = "inward", size = 3, check_overlap = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 










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



