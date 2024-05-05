library(readr)
library(tidyverse)

options(readr.show_col_types = FALSE)
options(warn = -1)

# Merge all the CSV files (5 in total) into one dataframe
df <- list.files(path = 'D:/HS/ISSS608-AY2023-24T4/Take-home_Ex/Take-home_Ex02/data', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

# Display unique values of the 'Purchaser Address Indicator' column
unique(df$`Purchaser Address Indicator`)

df <- df %>%
  filter(`Purchaser Address Indicator` == "Private")

df$`Sale Date` <- as.Date(df$`Sale Date`, format = "%d %b %Y")

# Filter data for March 2023 and March 2024
df <- df %>%
  filter(format(`Sale Date`, "%Y-%m") %in% c("2023-03", "2024-03"))

# Visualization 1
df_tran <- df %>%
  group_by(month = format(`Sale Date`, format = "%y'%m"), `Property Type`) %>%
  summarize(transactions = sum(`Number of Units`))

property_volumes <- df_tran %>%
  group_by(`Property Type`) %>%
  summarize(total_transactions = sum(transactions))

# Reorder levels of Property Type according to total transaction volumes
df_tran <- df_tran %>%
  mutate(`Property Type` = factor(`Property Type`, levels = property_volumes$`Property Type`[order(property_volumes$total_transactions, decreasing = TRUE)]))

# Calculate total transactions for March 2023 and March 2024 for each property type
total_transactions_march_2023 <- df_tran %>%
  filter(month == "23'03") %>%
  group_by(`Property Type`) %>%
  summarize(total_transactions_2023 = sum(transactions))

total_transactions_march_2024 <- df_tran %>%
  filter(month == "24'03") %>%
  group_by(`Property Type`) %>%
  summarize(total_transactions_2024 = sum(transactions))

# Merge total transactions data
total_transactions <- merge(total_transactions_march_2023, total_transactions_march_2024, by = "Property Type", all = TRUE)

# Calculate percentage difference
total_transactions <- total_transactions %>%
  mutate(percent_diff = ((total_transactions_2024 - total_transactions_2023) / total_transactions_2023) * 100)

# Filter for specific property types
total_transactions <- total_transactions %>%
  filter(`Property Type` %in% c("Condominium", "Apartment", "Executive Condominium"))

# Plot with sorted Property Type levels
ggplot(df_tran, aes(x = `month`, y = transactions, group = `Property Type`, fill = `Property Type`)) +
  geom_area(color = "black") +
  geom_text(data = total_transactions, aes(x = "24'03", y = total_transactions_2024, label = paste0(round(percent_diff, 2), "%")), vjust = -2, hjust = 0) +
  ggtitle("Transaction trend across different property types in March 2023 and March 2024") +
  xlab("Month") +
  ylab("No. of Transactions") +
  scale_fill_discrete()
