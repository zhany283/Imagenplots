library(ggplot2)

data <- data.frame(
  GroupName = c("BC001", "BC003", "BC004", "BC005", "BC006", "BC007", "BC008", 
                "BC009", "BC010", "BC011", "BC012", "BC013", "BC014", "BC015", "BC016"),
  SampleID = c("2022-BAL-15", "2027-BAL-00", "LT226-LUL-03", "LT243-Left", 
               "LT236-LLL", "LT121-Left", "LT121-Right", "LT122-RML", "LT205-LUL", 
               "2042-BAL-00", "2043-BAL-00", "LT181-L", "LT236-LUL", "2050-BAL-00", "2055-BAL-00"),
  NumberOfCells = c(2530, 279, 248, 291, 444, 645, 2254, 604, 1538, 1923, 781, 1285, 1408, 721, 821),
  UMICounts = c(4788, 429334, 350015, 110336, 22498, 19424, 1028073, 339277, 6345684, 3063079, 
                61009, 1365151, 2157984, 562742, 412863),
  PercentageTotalCells = c(16.04, 1.77, 1.57, 1.85, 2.82, 4.09, 14.29, 3.83, 9.75, 12.19, 4.95, 8.15, 8.93, 4.57, 5.21),
  PercentageTotalUMIs = c(0.03, 2.64, 2.15, 0.68, 0.14, 0.12, 6.32, 2.08, 39.00, 18.82, 0.37, 8.39, 13.26, 3.46, 2.54),
  MeanReadsPerCell = c(1.892490119, 1538.831541, 1411.350806, 379.161512, 50.67117117, 
                       30.11472868, 456.1104703, 561.7168874, 4125.93238, 1592.864795, 
                       78.11651729, 1062.374319, 1532.659091, 780.5020804, 502.8781973)
)

numeric_data <- data[, sapply(data, is.numeric)]
# Standardize the data before PCA
data.pca <- prcomp(numeric_data, scale. = TRUE)
# Extract the scores
scores <- as.data.frame(data.pca$x)
scores$GroupName <- data$GroupName
# Create a ggplot for the PCA plot
ggplot(scores, aes(x = PC1, y = PC2, label = GroupName)) +
  geom_point() +  # Add the points
  geom_text(aes(label=GroupName), vjust=2, color="red") +  # Add text labels for each point
  xlab("Principal Component 1") + 
  ylab("Principal Component 2") + 
  ggtitle("PCA Plot with Group Names") +
  theme_minimal()

  
  <img width="716" alt="BAL_PCA" src="https://github.com/zhany283/Imagenplots/assets/130387837/8868e158-78e4-4f1f-9604-d271389c0a02">





data <- data.frame(
  GroupName = c("BC001", "BC003", "BC004", "BC005", "BC006", "BC007", "BC008", "BC009", "BC010", "BC011", "BC012", "BC013", "BC014", "BC015", "BC016"),
  UMICounts = c(4788, 429334, 350015, 110336, 22498, 19424, 1028073, 339277, 6345684, 3063079, 61009, 1365151, 2157984, 562742, 412863),
  NeutrophilsProportion = c(96.5255, 87.5424, 83.9057, 84.3947, 94.8026, 94.1098, 98.3715, 92.8533, 1.2151, 70.957, 98.7948, 56.1643, 18.8539, 92.3502, 94.2184)
)
# Calculating Pearson correlation coefficient
correlation <- cor(data$UMICounts, data$NeutrophilsProportion, method = "pearson")
print(paste("Pearson correlation coefficient:", correlation))

# Generating the scatter plot with annotations
ggplot(data, aes(x = UMICounts, y = NeutrophilsProportion)) +
  geom_point() + # Add points
  scale_x_log10() + # Use logarithmic scale for X axis
  geom_text(aes(label = GroupName), hjust = 1.2, vjust = 1.2, size = 3) + # Add annotations
  geom_smooth(method = "lm", se = TRUE, color = "blue") + # Add linear trend line with confidence interval
  ylim(0, 100) + # Set y-axis limits
  labs(title = paste("Scatter plot of UMI counts vs Neutrophils proportion\nPearson correlation coefficient:", round(correlation, 2)),
       x = "UMI counts",
       y = "Neutrophils proportion (%)") +
  theme_minimal()
  <img width="1331" alt="corelation_pearson" src="https://github.com/zhany283/Imagenplots/assets/130387837/cba8191e-2d53-4a4b-a46f-8ad743723319">




