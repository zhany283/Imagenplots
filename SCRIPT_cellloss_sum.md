library(ggplot2)
library(dplyr)



# Calculate the median of cell loss after cryopreservation
data <- read.csv("/Users/ethan/Documents/SCRIPT_Meta_cellcount_data.csv")
median_cellloss <- median(data$cellloss, na.rm = TRUE)

> median_cellloss
[1] 0.425

# Convert Date format
data$Date <- as.POSIXct(data$Date, format = "%m/%d/%Y %H:%M")


# Dot plot cell loss% vs Date
ggplot(data, aes(x = Date, y = cellloss, color = Viability)) +
  geom_point(size = 2.5, alpha = 7) +
  theme_minimal() +
  labs(title = "Dotplot of cell loss by sample ID",
       x = "Sample ID", y = "Cell Loss (%)") +
      ylim(0.1,1)+
  #geom_text(aes(label = Sample_ID), vjust = "inward", hjust = "inward", size = 3, check_overlap = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(method = "lm")

