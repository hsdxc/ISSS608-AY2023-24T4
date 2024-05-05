library(readr)
library(tidyverse)
library(lubridate) # for handling dates

# Set options to avoid printing column types automatically and suppress warnings
options(readr.show_col_types = FALSE)
options(warn = -1)

# Read and merge CSV files
df <- list.files(path='D:/HS/ISSS608-AY2023-24T4/Take-home_Ex/Take-home_Ex02/data', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows()

# Filter data for a specific quarter
df_filtered <- df %>%
  mutate(`Sale Date` = dmy(`Sale Date`)) %>% 
  filter(year(`Sale Date`) == 2024, quarter(`Sale Date`) == 1)

# Create transaction summary
df_summary <- df_filtered %>%
  filter(`Purchaser Address Indicator` == "Private") %>%
  group_by(`Planning Region`, `Property Type`) %>%
  summarise(Transactions = sum(`Number of Units`), .groups = 'drop')

# Create heatmap visualization
ggplot(df_summary, aes(x = `Property Type`, y = `Planning Region`, fill = Transactions)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Transaction Heatmap by Planning Region and Property Type (Q1 2024)",
       x = "Property Type",
       y = "Planning Region",
       fill = "Number of Transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
