library(readxl)

# Load purchase behavior data
purchase_data <- read_excel("QVI_purchase_behaviour.xlsx")

# Load transaction data
transaction_data <- read_excel("QVI_transaction_data.xlsx")

# Structure of purchase_data
str(purchase_data)

# Summary statistics of purchase_data
summary(purchase_data)

# Structure of transaction_data
str(transaction_data)

# Summary statistics of transaction_data
summary(transaction_data)

# Summary statistics of numerical columns
summary(transaction_data[, c("PROD_QTY", "TOT_SALES")])

# Box plot of PROD_QTY
boxplot(transaction_data$PROD_QTY, main = "Box Plot of PROD_QTY")

# Box plot of TOT_SALES
boxplot(transaction_data$TOT_SALES, main = "Box Plot of TOT_SALES")

# Remove outliers in PROD_QTY and TOT_SALES based on a threshold (e.g., 99th percentile)
threshold_prod_qty <- quantile(transaction_data$PROD_QTY, 0.99)
threshold_tot_sales <- quantile(transaction_data$TOT_SALES, 0.99)

# Filter the dataframe to remove outliers
transaction_data_filtered <- transaction_data[transaction_data$PROD_QTY <= threshold_prod_qty & transaction_data$TOT_SALES <= threshold_tot_sales, ]

# Total spending on chips
total_spending <- aggregate(TOT_SALES ~ LYLTY_CARD_NBR, data = transaction_data, FUN = sum)

# Average spending per transaction
average_spending <- aggregate(TOT_SALES ~ LYLTY_CARD_NBR, data = transaction_data, FUN = mean)

# Frequency of chip purchases
purchase_frequency <- aggregate(TXN_ID ~ LYLTY_CARD_NBR, data = transaction_data, FUN = length)

# Preference for specific chip brands
brand_preference <- table(transaction_data$PROD_NBR)

# Preference for specific pack sizes
pack_size_preference <- table(transaction_data$PROD_NAME)

# Customer loyalty (number of transactions)
loyalty <- aggregate(TXN_ID ~ LYLTY_CARD_NBR, data = transaction_data, FUN = function(x) length(unique(x)))

# Basket analysis
basket <- table(transaction_data$LYLTY_CARD_NBR, transaction_data$PROD_NAME)

# Printing the results
print("Total spending on chips:")
print(total_spending)

print("Average spending per transaction:")
print(average_spending)

print("Frequency of chip purchases:")
print(purchase_frequency)

print("Preference for specific chip brands:")
print(brand_preference)

print("Preference for specific pack sizes:")
print(pack_size_preference)

print("Customer loyalty (number of transactions):")
print(loyalty)

print("Basket analysis:")
print(basket)

# Select relevant features for segmentation
segmentation_data <- transaction_data[, c("TOT_SALES", "PROD_QTY")]

# Standardize the data
standardized_data <- scale(segmentation_data)

# Determine the optimal number of clusters using the elbow method
wss <- numeric(10)
for (k in 1:10) {
  kmeans_model <- kmeans(standardized_data, centers = k)
  wss[k] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")

# Based on the elbow plot, choose the appropriate number of clusters
num_clusters <- 3

# Perform k-means clustering
kmeans_model <- kmeans(standardized_data, centers = num_clusters)

# Assign cluster labels to the original data
segmentation_data$cluster <- kmeans_model$cluster

# Analyze each segment's purchasing patterns
library(dplyr)
segment_summary <- segmentation_data %>%
  group_by(cluster) %>%
  summarise(
    Avg_Total_Spending = mean(TOT_SALES),
    Avg_Product_Quantity = mean(PROD_QTY)
  )

# Print the segment summary
print(segment_summary)

# Customer segmentation analysis

# Perform customer segmentation using clustering or segmentation algorithms
# Assign each customer to a specific segment based on their chip purchasing behavior

# Analyze each segment's purchasing patterns
segment_analysis <- aggregate(
  TOT_SALES ~ Segment + PROD_NAME + PACK_SIZE,
  data = transaction_data,
  FUN = function(x) c(Count = length(x), Total_Sales = sum(x))
)

# Identify significant drivers of spending and interesting trends within each segment
# You can use techniques like association rules, correlation analysis, or visualization to explore the data

# Generate strategic recommendation

# Recommendation 1: Targeted Marketing
# Provide a rationale for targeted marketing based on the analysis of customer segments
# Outline specific strategies for personalized promotions, product recommendations, and communication channels

# Recommendation 2: Product Optimization
# Provide a rationale for product optimization based on the analysis of purchasing patterns
# Discuss strategies for stocking preferred brands, offering different pack sizes, and introducing new chip products

# Recommendation 3: Loyalty Program Enhancement
# Provide a rationale for enhancing the loyalty program based on the analysis of customer loyalty
# Propose targeted incentives and rewards tailored to each segment

# Recommendation 4: Continuous Monitoring and Analysis
# Emphasize the importance of continuous monitoring and analysis to stay responsive to changing customer preferences
# Highlight the need for regular reviews, data updates, and iterative improvements

# Generate a report summarizing the recommendations and insights
report <- paste(
  "Strategic Recommendation:\n\n",
  "1. Targeted Marketing:\n",
  "   - Personalized promotions, product recommendations, and communication channels\n\n",
  "2. Product Optimization:\n",
  "   - Stocking preferred brands, offering different pack sizes, and introducing new chip products\n\n",
  "3. Loyalty Program Enhancement:\n",
  "   - Targeted incentives and rewards tailored to each segment\n\n",
  "4. Continuous Monitoring and Analysis:\n",
  "   - Regular reviews, data updates, and iterative improvements\n"
)

# Save the report as a text file or export it to a desired format
writeLines(report, "strategic_recommendation.txt")
