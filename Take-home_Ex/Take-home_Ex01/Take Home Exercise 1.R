# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

# Read individual CSV files
realis_1 <- read_csv("D:/HS/ISSS608-AY2023-24T4/Take-home_Ex/Take-home_Ex01/data/ResidentialTransaction20240308160536.csv")
realis_2 <- read_csv("D:/HS/ISSS608-AY2023-24T4/Take-home_Ex/Take-home_Ex01/data/ResidentialTransaction20240308160736.csv")
realis_3 <- read_csv("D:/HS/ISSS608-AY2023-24T4/Take-home_Ex/Take-home_Ex01/data/ResidentialTransaction20240308161009.csv")
realis_4 <- read_csv("D:/HS/ISSS608-AY2023-24T4/Take-home_Ex/Take-home_Ex01/data/ResidentialTransaction20240308161109.csv")
realis_5 <- read_csv("D:/HS/ISSS608-AY2023-24T4/Take-home_Ex/Take-home_Ex01/data/ResidentialTransaction20240414220633.csv")

# Combine all data frames into one
realis <- bind_rows(realis_1, realis_2, realis_3, realis_4, realis_5)

# Amend character types
realis$`Sale Date` <- as.Date(realis$`Sale Date`, format = "%d %b %Y")
realis$`Nett Price($)` <- as.numeric(gsub("[^0-9.]", "", realis$`Nett Price($)`))

# Filter first quarter of 2024
realis2024Q1 <- realis %>%
  filter(year(`Sale Date`) == 2024 & quarter(`Sale Date`) == 1)