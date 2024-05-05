library(readr)
library(tidyverse)

options(readr.show_col_types = FALSE)
options(warn=-1)

# Merge all the CSV files (5 in total) into one dataframe
df <- list.files(path='D:/HS/ISSS608-AY2023-24T4/Take-home_Ex/Take-home_Ex02/data', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

# Display unique values of the 'Purchaser Address Indicator' column
unique(df$`Purchaser Address Indicator`)

df <- df %>%
  filter(`Purchaser Address Indicator` == "Private")

df$`Sale Date` = as.Date(df$`Sale Date`, format = "%d %b %Y")

#Visualisation 1

df_tran = df %>%
  group_by(month=format(`Sale Date`, format="%y'%m"), `Property Type`) %>%
  summarize(transactions = sum(`Number of Units`))

df_tran_total = df %>%
  group_by(month=format(`Sale Date`, format="%y'%m")) %>%
  summarize(transactions = sum(`Number of Units`))
df_tran_total$`Property Type` = "Total"
df_tran <- rbind(df_tran, df_tran_total)

ggplot(df_tran, aes(x=`month`, y=`transactions`, group=`Property Type`, color=`Property Type`)) +
  geom_line() +
  ggtitle("Transaction trend across different property type")+
  xlab("Month")+
  ylab("No. of Transactions")

#Visualisation 2

df_unit_price = df %>%
  group_by(month=format(`Sale Date`, format="%y'%m"), `Property Type`) %>%
  summarize(`Unit Price ($ PSF)` = mean(`Unit Price ($ PSF)`))

ggplot(df_unit_price, aes(x=`month`, y=`Unit Price ($ PSF)`, group=`Property Type`, color=`Property Type`)) +
  geom_line() +
  ggtitle("Unit Price (PSF) across different property type")+
  xlab("Month")+
  ylab("Mean Unit Price ($ PSF)")

#Visualisation 3

unique(df$`Planning Region`)

df_tran_area = df %>%
  group_by(month=format(`Sale Date`, format="%y'%m"), `Planning Region`) %>%
  summarize(transactions = sum(`Number of Units`))

