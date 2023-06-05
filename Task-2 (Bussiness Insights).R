# Load the required libraries
library(dplyr)
library(ggplot2)

# Load the QVI_data dataset
QVI_data <- read.csv("QVI_data.csv")

# Filter the data for the trial period
trial_period <- QVI_data %>% filter(STORE_NBR %in% c(77, 86, 88))

# Select the control stores
control_stores <- QVI_data %>% 
  filter(STORE_NBR != 77 & STORE_NBR != 86 & STORE_NBR != 88) %>%
  distinct(STORE_NBR)

# Calculate total sales revenue for each store
total_sales <- trial_period %>% 
  group_by(STORE_NBR) %>% 
  summarize(total_sales = sum(TOT_SALES))

# Calculate total number of customers for each store
total_customers <- trial_period %>% 
  group_by(STORE_NBR) %>% 
  summarize(total_customers = n_distinct(LYLTY_CARD_NBR))

# Calculate average number of transactions per customer for each store
avg_transactions <- trial_period %>% 
  group_by(STORE_NBR) %>% 
  summarize(avg_transactions = n() / n_distinct(LYLTY_CARD_NBR))

# Function to calculate the magnitude distance
magnitude_distance <- function(observed, minimum, maximum) {
  1 - (observed - minimum) / (maximum - minimum)
}

# Calculate the magnitude distance for each store
control_metrics <- control_stores %>% 
  left_join(total_sales, by = "STORE_NBR") %>%
  left_join(total_customers, by = "STORE_NBR") %>%
  left_join(avg_transactions, by = "STORE_NBR") %>%
  mutate(
    total_sales_md = magnitude_distance(total_sales, min(total_sales, na.rm = TRUE), max(total_sales, na.rm = TRUE)),
    total_customers_md = magnitude_distance(total_customers, min(total_customers, na.rm = TRUE), max(total_customers, na.rm = TRUE)),
    avg_transactions_md = magnitude_distance(avg_transactions, min(avg_transactions, na.rm = TRUE), max(avg_transactions, na.rm = TRUE))
  )

# Merge the trial stores with the control metrics
trial_metrics <- control_metrics %>%
  filter(STORE_NBR %in% c(77, 86, 88))

# Print the trial metrics
print(trial_metrics)


# Perform t-test for total sales
ttest_total_sales <- t.test(trial_store$TOT_SALES, control_stores$TOT_SALES)

# Print the t-test results
print(ttest_total_sales)

---output
Welch Two Sample t-test

data:  trial_store$TOT_SALES and control_stores$TOT_SALES
t = -15.371, df = 3374.8, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -1.096400 -0.848338
sample estimates:
  mean of x mean of y 
7.551233  8.523602 

# Subset the data for the trial store and control stores during the trial period
trial_period <- QVI_data[QVI_data$STORE_NBR %in% c(77, 86, 88) & QVI_data$DATE >= as.Date("2019-02-01") & QVI_data$DATE <= as.Date("2019-04-30"), ]

# Calculate the total number of unique customers for the trial store
trial_store_customers <- n_distinct(trial_period$LYLTY_CARD_NBR)

# Calculate the total number of unique customers for each control store
control_store_customers <- trial_period %>%
  group_by(STORE_NBR) %>%
  summarise(total_customers = n_distinct(LYLTY_CARD_NBR))

# Print the results
print("Total number of purchasing customers:")
print(trial_store_customers)
print(control_store_customers)


restart the proccess ones again

library(dplyr)

# Calculate total number of purchasing customers for trial store
trial_customers <- QVI_data %>%
  filter(STORE_NBR %in% c(77, 86, 88)) %>%
  distinct(LYLTY_CARD_NBR) %>%
  summarise(total_customers = n())

trial_customers

trial_avg_transactions <- QVI_data %>%
  filter(STORE_NBR %in% c(77, 86, 88)) %>%
  group_by(LYLTY_CARD_NBR) %>%
  summarise(total_transactions = n()) %>%
  summarise(avg_transactions = mean(total_transactions))

trial_avg_transactions

# Calculate average number of transactions per customer for trial store
trial_avg_transactions <- QVI_data %>%
  filter(STORE_NBR %in% c(77, 86, 88)) %>%
  group_by(LYLTY_CARD_NBR) %>%
  summarise(total_transactions = n()) %>%
  summarise(avg_transactions = mean(total_transactions))

# Calculate average number of transactions per customer for control stores 77, 86, and 88
control_avg_transactions <- QVI_data %>%
  filter(STORE_NBR %in% c(77, 86, 88)) %>%
  group_by(STORE_NBR, LYLTY_CARD_NBR) %>%
  summarise(total_transactions = n()) %>%
  group_by(STORE_NBR) %>%
  summarise(avg_transactions = mean(total_transactions))

trial_avg_transactions
control_avg_transactions

TIME TO VISUALIZATION..

library(ggplot2)

# Create a data frame combining trial and control metrics
comparison_data <- data.frame(
  Store_Type = c("Trial", "Control"),
  Metric = c("Total Customers", "Average Transactions"),
  Value = c(trial_customers$total_customers, control_avg_transactions$avg_transactions)
)

# Create a bar chart for the total number of customers
total_customers_chart <- ggplot(data = comparison_data, aes(x = Store_Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Number of Customers Comparison", x = "Store Type", y = "Total Customers") +
  theme_minimal()

# Create a bar chart for the average transactions per customer
avg_transactions_chart <- ggplot(data = comparison_data, aes(x = Store_Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Transactions per Customer Comparison", x = "Store Type", y = "Average Transactions") +
  theme_minimal()

# Display the charts
total_customers_chart
avg_transactions_chart

