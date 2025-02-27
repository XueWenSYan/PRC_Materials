# Load necessary libraries
library(ggplot2)
library(dplyr)
library(scales)
library(readr)

# Load the data
data <- read_csv("/Users/xy4889/Downloads/Data in format for Shelly to Make Graphs.csv")

# Define the desired journal order
journal_order <- c("Amer J Sociol", "Amer Sociol Rev", 
                   "Chin Sociol Rev", "Brit J Sociol", 
                   "Cana Sociol Rev", "Amer J Poli Sci", 
                   "Amer Poli Sci Rev", "Chin Poli Sci Rev", 
                   "Brit J Poli Sci", "Swis Poli Sci Rev")

# Set Jour as a factor with the specified levels
data$Jour <- factor(data$Jour, levels = journal_order)

# Ensure the year column is a character to match with dummy row
data$year <- as.character(data$year)

# Calculate the sample size (total count) for each year
sample_sizes <- data %>%
  group_by(year) %>%
  summarise(N = sum(Native + General + Foreign))

# Merge the sample sizes with the original data
data <- data %>%
  left_join(sample_sizes, by = "year")

# Reshape the data to long format for easier plotting with ggplot2
data_long <- data %>%
  pivot_longer(cols = c("Native", "General", "Foreign"), names_to = "Category", values_to = "Count")

# Add a dummy row for the placeholder facet
dummy_row <- data.frame(
  Jour = factor("Amer J Sociol", levels = journal_order),  # Arbitrary journal for dummy
  year = "dummy",
  Category = c("Native", "General", "Foreign"),
  Count = 0
)

# Bind the dummy row to the data
data_long <- bind_rows(data_long, dummy_row)

# Set year as a factor with sample size in the facet label, including the dummy year in the specified order
data_long$year <- factor(data_long$year, levels = c("2019", "2020", "dummy", "2021", "2022", "2023"),
                         labels = c("2019 (N = 352)", "2020 (N = 400)", "", "2021 (N = 430)", "2022 (N = 445)", "2023 (N = 419)"))

# Create the plot
ggplot(data_long, aes(x = Jour, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ year, nrow = 2, ncol = 3) +  # Set number of rows and columns for the facet layout
  labs(title = "Percentage of Articles in Nation-Branded Journals Analyzing Domestic Society, a Foreign\n Society or Many Societies/Society in General",
       y = "Percentage of Articles",
       x = NULL) +
  scale_y_continuous(labels = percent_format(scale = 1)) +  # Format y-axis as percentages
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +  # Show all journal names, adjust if overlapping
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
