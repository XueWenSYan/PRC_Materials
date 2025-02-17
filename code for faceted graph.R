library(ggplot2)
library(tidyverse)
library(scales)
data <- read_csv("C:/Users/xwuey/OneDrive/DESKTOP/Data in format for Shelly to Make Graphs.csv")

data$Jour |> unique()

journal_order <- c("Amer J Sociol", "Amer Sociol Rev", 
                   "Chin Sociol Rev", "Brit J Sociol", 
                   "Cana Sociol Rev", "Amer J Poli Sci", 
                   "Amer Poli Sci Rev", "Chin Poli Sci Rev", 
                   "Brit J Poli Sci", "Swis Poli Sci Rev")

# Set Journal as a factor with the specified levels
data$Jour <- factor(data$Jour, levels = journal_order)



sample_sizes <- data %>%
  group_by(year) %>%
  summarise(N = sum(Native + General + Foreign))

# Merge the sample sizes with the original data
data <- data %>%
  left_join(sample_sizes, by = "year")

# Reshape the data to long format for easier plotting with ggplot2
data_long <- data %>%
  pivot_longer(cols = c("Native", "General", "Foreign"), names_to = "Category", values_to = "Count")

data_long$year <- factor(data_long$year, levels = unique(data_long$year),
                         labels = paste(unique(data_long$year), "(N =", sample_sizes$N, ")"))

# Create the plot
ggplot(data_long, aes(x = Jour, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ year) +  # Facet by year, with modified labels
  labs(title = "Percentage of Articles in Nation-Branded Journals Analyzing Domestic Society, a Foreign\n Society or Many Societies/Society in General (Total N = 2,046)",
       y = "Percentage of Articles",
       x = NULL) +
  scale_y_continuous(labels = percent_format(scale = 1)) +  # Format y-axis as percentages
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
