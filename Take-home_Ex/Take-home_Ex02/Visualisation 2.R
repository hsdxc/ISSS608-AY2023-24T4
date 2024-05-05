# Load necessary libraries
library(readr)
library(tidyverse)

# Set options
options(readr.show_col_types = FALSE)
options(warn=-1)

# Merge all the CSV files (5 in total) into one dataframe
df <- list.files(path='D:/HS/ISSS608-AY2023-24T4/Take-home_Ex/Take-home_Ex02/data', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

# Display unique values of the 'Purchaser Address Indicator' column
unique(df$`Purchaser Address Indicator`)

# Filter data for only private purchasers
df <- df %>%
  filter(`Purchaser Address Indicator` == "Private")

# Convert 'Sale Date' column to Date format
df$`Sale Date` <- as.Date(df$`Sale Date`, format = "%d %b %Y")

# Visualisation: Unit Price across different property types over months
df_unit_price <- df %>%
  group_by(month = format(`Sale Date`, format="%y-%m"), `Property Type`) %>%
  summarize(`Unit Price ($ PSF)` = mean(`Unit Price ($ PSF)`))

# Filter data for only Quarter 1 2024
df_filtered <- df %>%
  filter(format(`Sale Date`, "%Y-%m") %in% c("2024-01","2024-02","2024-03"))

# Calculate average unit price for each property type for March 2023 and March 2024
avg_prices <- df_filtered %>%
  group_by(`Property Type`) %>%
  summarize(`Average Unit Price ($ PSF)` = mean(`Unit Price ($ PSF)`))

# Convert 'Sale Date' column in df_filtered to Date format
df_filtered$`Sale Date` <- as.Date(df_filtered$`Sale Date`, format = "%Y-%m-%d")

# Visualisation: Unit Price across different property types for March 2023 and March 2024 (faceted)
ggplot(df_filtered, aes(x = `Sale Date`, y = `Unit Price ($ PSF)`, color = `Property Type`)) +
  geom_line() +
  facet_wrap(~`Property Type`, scales = "free_y") +  # Faceting by property type
  geom_text(data = avg_prices, aes(label = paste("Avg:", round(`Average Unit Price ($ PSF)`, 2)), x = max(df_filtered$`Sale Date`, na.rm = TRUE), y = Inf), hjust = 1, vjust = 1, size = 3) + # Add average price annotations
  geom_hline(data = avg_prices, aes(yintercept = `Average Unit Price ($ PSF)`, color = `Property Type`), linetype = "dashed", color = "black") +  # Add horizontal mean lines
  ggtitle("Unit Price (PSF) across different property types for Quarter 1 2024") +
  xlab("Sale Date") +
  ylab("Mean Unit Price ($ PSF)")


