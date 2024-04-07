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
### Outliners were removed from analysis - (not sorted/ no post-sort counting/ cell loss% fall into negative range)
ggplot(data, aes(x = Date, y = cellloss, colour = Viability)) +
  geom_point(size = 2.5, alpha = 1) +
  theme_minimal() +
  labs(title = "Dotplot of cell loss over time",
       x = "Date", y = "Cell Loss (%)") + 
  ylim(0.1,1) + 
  scale_colour_gradient(low = "darkred", high = "cyan") + 
  #geom_text(aes(label = Sample_ID), vjust = "inward", hjust = "inward", size = 3, check_overlap = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #geom_smooth(method = "lm")

![image](https://github.com/zhany283/Imagenplots/assets/130387837/57952c75-48f6-4ca7-b796-0fe6fc225c35)



# Add Trend line
ggplot(data, aes(x = Date, y = cellloss, colour = Viability)) +
  geom_point(size = 2.5, alpha = 1) +
  theme_minimal() +
  labs(title = "Dotplot of cell loss over time",
       x = "Date", y = "Cell Loss (%)") + 
  ylim(0.1,1) + 
  scale_colour_gradient(low = "darkred", high = "cyan") + 
  #geom_text(aes(label = Sample_ID), vjust = "inward", hjust = "inward", size = 3, check_overlap = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(method = "lm")

![image](https://github.com/zhany283/Imagenplots/assets/130387837/eb9b50cd-2250-4aa3-9f6a-912ed9170c4a)

