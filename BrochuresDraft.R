library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)

##EFCITtest <- read_csv("Downloads/EFCITtest.csv")
EFCITtest <- read_csv("Downloads/EFCITtest2.csv")
view(EFCITtest)
# Load the dataset
data <- EFCITtest2

# Convert Week column to Date type and ensure proper ordering
data <- data %>%
  mutate(Week = as.character(Week)) %>%  # Ensure it's a character
  mutate(Week = mdy(Week)) %>%  # MM/DD/YY format parsing
  drop_na(Week) %>%  # Remove any NA values that result from conversion errors
  arrange(Week)  # Arrange in ascending order

# Debugging step: Print a preview of Week column to check formatting issues
print(unique(data$Week))

##### "Should I participate in a Clinical Trial?" 

# Filter and sort before plotting
filtered_data <- data %>%
  filter(`Brochure Name/ID` %in% c("Should I participate in a clinical trial", "Deberia participar en un ensayo clinico?")) %>%
  arrange(Week)

# Create the plot
ggplot(filtered_data, aes(x = Week, y = `Stock Used`, color = `Brochure Name/ID`, group = `Brochure Name/ID`)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_date(breaks = unique(filtered_data$Week), date_labels = "%m/%d/%y") +
  labs(title = "Brochures Distributed Per Week (Galter20) - Clinical Trials (Updated)",
       x = "Week Ending",
       y = "Stock Used",
       color = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

##### "Finding Treatments Together AA" 

# Filter and sort before plotting
filtered_data <- data %>%
  filter(`Brochure Name/ID` %in% c("Encontrando tratamientos juntos AA", "Finding Treatments together AA")) %>%
  arrange(Week)

# Create the plot
ggplot(filtered_data, aes(x = Week, y = `Stock Used`, color = `Brochure Name/ID`, group = `Brochure Name/ID`)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Encontrando tratamientos juntos AA" = "blue", "Finding Treatments together AA" = "green")) +
  scale_x_date(breaks = unique(filtered_data$Week), date_labels = "%m/%d/%y") +
  labs(title = "Brochures Distributed Per Week (Galter20) - AA (Updated)",
       x = "Week Ending",
       y = "Stock Used",
       color = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

##### "Finding Treatments Together Hispanics" 

# Filter and sort before plotting
filtered_data <- data %>%
  filter(`Brochure Name/ID` %in% c("Encontrando tratamientos juntos Hispanos", "Finding Treatments together Hispanic")) %>%
  arrange(Week)

# Create the plot
ggplot(filtered_data, aes(x = Week, y = `Stock Used`, color = `Brochure Name/ID`, group = `Brochure Name/ID`)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Encontrando tratamientos juntos Hispanos" = "orange", "Finding Treatments together Hispanic" = "gold")) +
  scale_x_date(breaks = unique(filtered_data$Week), date_labels = "%m/%d/%y") +
  labs(title = "Brochures Distributed Per Week (Galter20) - Hispanic (Updated)",
       x = "Week Ending",
       y = "Stock Used",
       color = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

##### ALL TOGETHER

# Define colors for each brochure
target_colors <- c("Deberia participar en un ensayo clinico?" = "orange",
                   "Encontrando tratamientos juntos AA" = "brown",
                   "Encontrando tratamientos juntos Hispanos" = "red",
                   "Finding Treatments together AA" = "pink",
                   "Finding Treatments together Hispanic" = "skyblue",
                   "Should I participate in a clinical trial" = "blue")

# Sort before plotting
data <- data %>% arrange(Week)

# Create the plot
ggplot(data, aes(x = Week, y = `Stock Used`, color = `Brochure Name/ID`, group = `Brochure Name/ID`)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = target_colors) +
  scale_x_date(breaks = unique(filtered_data$Week), date_labels = "%m/%d/%y") +
  labs(title = "Brochures Distributed Per Week by Type",
       x = "Week Ending",
       y = "Stock Used",
       color = "Brochure Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

