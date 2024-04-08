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




# Median for each year;
data$Period <- cut(data$Date,
                   breaks = as.POSIXct(c('2021-01-01', '2022-01-01', '2023-01-01', '2024-01-01')),
                   labels = c('2021-2022', '2022-2023', '2023-2024'),
                   include.lowest = TRUE)

data$Period <- factor(data$Period, levels = c(levels(data$Period), "before 2021"))

data$Period[is.na(data$Period)] <- "before 2021"

median_cellloss_per_period <- data %>%
  group_by(Period) %>%
  summarize(MedianCellLoss = median(cellloss, na.rm = TRUE))

data$Period <- factor(data$Period, levels = c("before 2021", "2021-2022", "2022-2023", "2023-2024"))

median_cellloss_per_period <- data %>%
  group_by(Period) %>%
  summarize(MedianCellLoss = median(cellloss, na.rm = TRUE))

print(median_cellloss_per_period)

![image](https://github.com/zhany283/Imagenplots/assets/130387837/6a856d82-cf8b-4bfe-91bf-1b7d292dfba2)








# Add median line for each timeframe and label
 median_cellloss_per_period <- data.frame(
   Period = c('2021-2022', '2022-2023', '2023-2024'),
   MedianCellLoss = c(0.35, 0.55, 0.62), 
   StartDate = as.Date(c('2021-01-01', '2022-01-01', '2023-01-01')),
   EndDate = as.Date(c('2021-12-31', '2022-12-31', '2023-12-31'))
 )

 median_cellloss_per_period$StartDate <- as.POSIXct(median_cellloss_per_period$StartDate, format = "%Y-%m-%d", tz = "UTC")
 median_cellloss_per_period$EndDate <- as.POSIXct(median_cellloss_per_period$EndDate, format = "%Y-%m-%d", tz = "UTC") + lubridate::days(1) - seconds(1) 

ggplot(data, aes(x = Date, y = cellloss)) +
  geom_point(aes(colour = Viability), size = 2.5, alpha = 1) + 
  theme_minimal() +
  labs(title = "SCRIPT Dotplot of cell loss over time", x = "Date", y = "Cell Loss (%)") +
  ylim(0.1, 1) +
  scale_colour_gradient(low = "darkred", high = "cyan") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(method = "lm", aes(colour = Viability)) +
  geom_segment(data = median_cellloss_per_period, aes(x = StartDate, xend = EndDate, y = MedianCellLoss, yend = MedianCellLoss), colour = "black", linetype = "dashed") +
  geom_text(data = median_cellloss_per_period, aes(x = EndDate, y = MedianCellLoss, label = Period), hjust = 1, vjust = -0.5, size = 5)

  ![image](https://github.com/zhany283/Imagenplots/assets/130387837/f881f38b-f507-46f8-9cb9-a835b2f66524)


![image](https://github.com/zhany283/Imagenplots/assets/130387837/6220dd82-4e78-4737-9180-c59cd6fe40c1)


# Anova analysis
# remove negative value
data_filtered <- data %>%
  filter(cellloss >= 0)
# Shapiro-test to verify normal distribution
shapiro_test_results <- by(data_filtered$cellloss, data_filtered$Period, shapiro.test)
print(shapiro_test_results)

![image](https://github.com/zhany283/Imagenplots/assets/130387837/6299dd2d-dc4d-4ad1-b37e-6c8cdb4b652d)

#Total number for 2022-2023 and 2023-2024
sample_counts <- data_filtered %>%
  filter(Period %in% c("2022-2023", "2023-2024")) %>%
  group_by(Period) %>%
  summarize(TotalSamples = n())
  
![image](https://github.com/zhany283/Imagenplots/assets/130387837/8243d78a-0c7a-4f4a-b395-877811f705b9)




