# Load necessary libraries
library(ggplot2)

# Sort the data by transaction cost
realis <- realis[order(-realis$`Transacted Price ($)`), ]

# Calculate cumulative percentage of total transaction cost
realis$cumulative_percentage <- cumsum(realis$`Transacted Price ($)`) / sum(realis$`Transacted Price ($)`)

# Create Pareto chart
pareto_chart <- ggplot(realis, aes(x = reorder(`Project Name`, -`Transacted Price ($)`), y = `Transacted Price ($)`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(y = cumulative_percentage), color = "red", group = 1) +
  geom_point(aes(y = cumulative_percentage), color = "red") +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Cumulative Percentage")) +
  labs(title = "Pareto Chart of Transaction Cost",
       x = "Project Name",
       y = "Transaction Cost ($)",
       color = "Cumulative Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Print the chart
print(pareto_chart)
