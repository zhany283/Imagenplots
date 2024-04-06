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














