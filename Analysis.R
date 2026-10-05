install.packages("readr")     #CSV file read
install.packages("dplyr")    #data filter,arrange,select(manipulation)
install.packages("ggplot2")  #graph/visualization
install.packages("lubridate") #date time handle
install.packages("skimr")  #to see dataset summery 
install.packages("corrplot") #co-relation
install.packages("caTools") # split 80:20
install.packages("randomForest") #random forest model
install.packages("xgboost")  # XGboost model
install.packages("cluster") #k-means clustering


# STEP 1 : Load Required Libraries
#=============================================================
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(skimr)
library(corrplot)
library(caTools)
library(randomForest)
library(xgboost)
library(cluster)

# STEP 2 : Import Dataset
#=============================================================
data <- read_csv("orders_export_1.csv")

head(data) # to see first 6 row
colnames(data) #show all column name
str(data)  #structure of data

#For find the problem
problems(data)

#Identify Column
View(data)


# STEP 3 : Select Required Column
#=============================================================
research_data <- data %>%
  select(
    Name,
    Email,
    `Created at`,
    `Shipping City`,
    `Lineitem sku`,
    `Lineitem name`,
    `Lineitem quantity`,
    `Lineitem price`,
    `Discount Amount`,
    Shipping,
    Total
  )

#Rename the column--------------
research_data <- research_data %>%
  rename(
    Order_ID = Name,
    Customer_ID = Email,
    Order_Date = `Created at`,
    Region = `Shipping City`,
    Product_ID = `Lineitem sku`,
    Product_Name = `Lineitem name`,
    Quantity = `Lineitem quantity`,
    Selling_Price = `Lineitem price`,
    Discount = `Discount Amount`,
    Shipping_Cost = Shipping,
    Sales_Amount = Total
  )

#Check dataset---------
head(research_data)
str(research_data)
summary(research_data)  #minimum,maximum,median,mode
dim(research_data)  #total row column number

#save new dataset(1)------------------------------------
write.csv(
  research_data,
  "research_dataset.csv",
  row.names = FALSE    #no extra row save
)

#step 4 :row reduce
#=============================================================
set.seed(123)

research_data <- research_data %>%
  sample_n(5000)

#Check row column number---------
dim(research_data)

#save data(2)----------
write.csv(
  research_data,
  "research_dataset.csv",
  row.names = FALSE
)

#step 5: missing value check/analysis-
#=============================================================
colSums(is.na(research_data))

#missing value handle------------------------
research_data$Customer_ID[is.na(research_data$Customer_ID)] <- "Guest Customer"

research_data$Region[is.na(research_data$Region)] <- "Unknown"

research_data$Product_ID[is.na(research_data$Product_ID)] <- "Unknown SKU"

research_data$Discount[is.na(research_data$Discount)] <- 0

research_data$Shipping_Cost[is.na(research_data$Shipping_Cost)] <- 0

research_data$Sales_Amount[is.na(research_data$Sales_Amount)] <-
  research_data$Selling_Price[is.na(research_data$Sales_Amount)] *
  research_data$Quantity[is.na(research_data$Sales_Amount)]

#again check missing data------------
colSums(is.na(research_data))


#Step 6: duplicate check
#=============================================================
sum(duplicated(research_data))

#unique product name---------------
length(unique(research_data$Product_Name))

#step 7: Add Dummy Inventory Columns
#=============================================================
set.seed(123)

research_data <- research_data %>%
  mutate(
    Cost_Price = round(Selling_Price * runif(n(), 0.70, 1.20), 0),
    Stock = sample(5:120, n(), replace = TRUE),
    Return_Count = sample(0:5, n(), replace = TRUE)
  )


#step 8: Feature Engineering
#=============================================================
research_data <- research_data %>%
  mutate(
    Profit = Sales_Amount - (Cost_Price * Quantity) - Shipping_Cost,
    Profit_Status = ifelse(Profit >= 0, "Profit", "Loss"),
    Profit_Margin = round((Profit / Sales_Amount) * 100, 2)
  )
#check-------------
head(research_data)
summary(research_data$Profit)

table(research_data$Profit_Status) #to see loss/profit number

#save data 3rd time-------------
write.csv(
  research_data,
  "research_dataset_final.csv",
  row.names = FALSE
)

View(research_data)


#step 9: Convert Order Date
#=============================================================

research_data$Order_Date <- ymd_hms(research_data$Order_Date)
class(research_data$Order_Date)  #for checking date type

#Step 10: Create Month, Year and Season
#=============================================================

research_data$Month <- month(research_data$Order_Date, label = TRUE)
research_data$Year <- year(research_data$Order_Date)

research_data$Season <- ifelse(
  research_data$Month %in% c("Mar","Apr","May","Jun"),
  "Summer",
  
  ifelse(
    research_data$Month %in% c("Jul","Aug","Sep"),
    "Rainy",
    
    ifelse(
      research_data$Month %in% c("Oct","Nov"),
      "Autumn",
      "Winter"
    )
  )
)

# Check month, season-----------
head(research_data) #first 6 row
table(research_data$Month) #data number under per month
table(research_data$Season) #data under per season


#Step-11: Save Final Dataset
#===============================================
write.csv(
  research_data,
  "research_dataset_final.csv",
  row.names = FALSE
)

View(research_data)
tail(research_data)


#=============================================================
# STEP 12 : Sales Analysis
#=============================================================

# 1.Total Sales Summary--------------------
sales_summary <- research_data %>%
  summarise(
    Total_Orders = n(),
    Total_Sales = sum(Sales_Amount, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE),
    Average_Sales_Value = mean(Sales_Amount, na.rm = TRUE),
    Average_Profit = mean(Profit, na.rm = TRUE)
  )
sales_summary

# 2.Monthly sales analysis----------------
monthly_sales <- research_data %>%
  group_by(Year, Month) %>%
  summarise(
    Total_Sales = sum(Sales_Amount, na.rm = TRUE),
    Total_Orders = n(),
    .groups = "drop"
  )
monthly_sales
#graph...............
ggplot(monthly_sales,
       aes(x = Month,
           y = Total_Sales,
           group = Year)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  labs(
    title = "Monthly Sales Trend",
    x = "Month",
    y = "Total Sales"
  )

# 3.region wise sales--------------------------
research_data$Region[research_data$Region %in% c("Dhaka", "dhaka", "ঢাকা")] <- "Dhaka"
research_data$Region[research_data$Region %in% c("Chittagong", "Chattogram")] <- "Chittagong"

region_sales <- research_data %>%
  group_by(Region) %>%
  summarise(
    Total_Sales = sum(Sales_Amount, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)
region_sales

#graph...........................
options(scipen = 999) #to skip scientific notation
ggplot(region_sales,
       aes(x = reorder(Region, Total_Sales),
           y = Total_Sales)) +
  
  geom_col(fill = "steelblue") +
  
  labs(
    title = "Top 10 Region-wise Sales",
    x = "Region",
    y = "Total Sales"
  ) + theme_minimal()

#discount analysis----------------------
discount_analysis <- research_data %>%
  group_by(Discount) %>%
  summarise(
    Average_Sales = mean(Sales_Amount, na.rm = TRUE),
    Total_Orders = n(),
    .groups = "drop"
  )
discount_analysis

#graph......................
ggplot(discount_analysis,
       aes(x = Discount,
           y = Average_Sales)) +
  
  geom_point(size = 2) +
  geom_line(linewidth=1) +
  
  labs(
    title = "Discount vs Average Sales",
    x = "Discount",
    y = "Average Sales"
  )

#=============================================================
# STEP 13 : Customer Behaviour Analysis
#=============================================================

# 1.top 10 customer by sales-------------
customer_sales <- research_data %>%
  group_by(Customer_ID) %>%
  summarise(
    Total_Sales = sum(Sales_Amount, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)
customer_sales

#graph...................
ggplot(customer_sales,
       aes(x = reorder(Customer_ID, Total_Sales),
           y = Total_Sales)) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Customers by Sales",
    x = "Customer",
    y = "Total Sales"
  ) +
  
  theme_minimal()

# 2.top 10 region by orders-----------------------
region_orders <- research_data %>%
  group_by(Region) %>%
  summarise(
    Total_Orders = n()
  ) %>%
  arrange(desc(Total_Orders)) %>%
  head(10)

region_orders

#graph..............
ggplot(region_orders,
       aes(x = reorder(Region, Total_Orders),
           y = Total_Orders)) +
  
  geom_col(fill = "steelblue") +
  
  labs(
    title = "Top 10 Regions by Orders",
    x = "Region",
    y = "Number of Orders"
  ) +
  
  theme_minimal()

# 3.season wise customer-------------------------
season_customer <- research_data %>%
  group_by(Season) %>%
  summarise(
    Customers = n_distinct(Customer_ID)
  )

season_customer

#graph.................
ggplot(season_customer,
       aes(x = Season,
           y = Customers)) +
  
  geom_col(fill = "steelblue") +
  
  labs(
    title = "Customers by Season",
    x = "Season",
    y = "Customers"
  ) +
  
  theme_minimal()

# 4.Average purchase per customer-----------------------
avg_purchase <- research_data %>%
  group_by(Customer_ID) %>%
  summarise(
    Average_Purchase = mean(Sales_Amount, na.rm = TRUE)
  ) %>%
  arrange(desc(Average_Purchase)) %>%
  head(10)

avg_purchase

#graph...............
ggplot(avg_purchase,
       aes(x = reorder(Customer_ID, Average_Purchase),
           y = Average_Purchase)) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Customers by Average Purchase",
    x = "Customer",
    y = "Average Purchase"
  ) +
  
  theme_minimal()


#=============================================================
# STEP 14 : Profit Monitoring
#=============================================================

# 1.profit summary-------------
profit_summary <- research_data %>%
  summarise(
    Total_Profit = sum(Profit, na.rm = TRUE),
    Average_Profit = mean(Profit, na.rm = TRUE),
    Maximum_Profit = max(Profit, na.rm = TRUE),
    Minimum_Profit = min(Profit, na.rm = TRUE)
  )

profit_summary

# 2.Monthly Profit Analysis------------
monthly_profit <- research_data %>%
  group_by(Year, Month) %>%
  summarise(
    Total_Profit = sum(Profit, na.rm = TRUE),
    .groups = "drop"
  )

monthly_profit

#graph.........
ggplot(monthly_profit,
       aes(x = Month,
           y = Total_Profit,
           group = Year)) +
  
  geom_line(linewidth = 1) +
  
  geom_point(size = 2) +
  
  labs(
    title = "Monthly Profit Trend",
    x = "Month",
    y = "Total Profit"
  ) + theme_minimal()

# 3.Top 10 region wise profit--------------
region_profit <- research_data %>%
  group_by(Region) %>%
  summarise(
    Total_Profit = sum(Profit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Profit)) %>%
  head(10)

region_profit

#graph....................
ggplot(region_profit,
       aes(x = reorder(Region, Total_Profit),
           y = Total_Profit)) +
  
  geom_col(fill = "steelblue") +
  
  labs(
    title = "Top 10 Region-wise Profit",
    x = "Region",
    y = "Profit"
  ) +
  
  theme_minimal()


# 4.Profit Status----------------
profit_status <- research_data %>%
  group_by(Profit_Status) %>%
  summarise(
    Total_Orders = n(),
    .groups = "drop"
  )

profit_status

#graph.........
ggplot(profit_status,
       aes(x = Profit_Status,
           y = Total_Orders,
           fill = Profit_Status)) +
  
  geom_col() +
  
  labs(
    title = "Profit Status",
    x = "Profit Status",
    y = "Total Orders"
  ) +
theme_minimal()

# 5.Profit margin Distribution-------------------(profit/sales amount)*100
research_data$Profit_Margin[is.infinite(research_data$Profit_Margin)] <- NA
summary(research_data$Profit_Margin)
# Graph---------------

ggplot(research_data,
       aes(x = Profit_Margin)) +
  
  geom_histogram(
    bins = 30,            #interval
    fill = "steelblue",
    color = "black"
  ) +
  
  labs(
    title = "Profit Margin Distribution",
    x = "Profit Margin (%)",
    y = "Order NO"
  ) +
  
  theme_minimal()

#=============================================================
# STEP 15 : Product Performance Evaluation
#=============================================================

# Top 10 Products by Sales-------------
product_sales <- research_data %>%
  group_by(Product_Name) %>%
  summarise(
    Total_Sales_Amount = sum(Sales_Amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales_Amount)) %>%
  head(10)

product_sales

#graph............................
ggplot(product_sales,
       aes(x = reorder(Product_Name, Total_Sales_Amount),
           y = Total_Sales_Amount)) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Products by Sales",
    x = "Product",
    y = "Total Sales Amount"
  ) +
theme_minimal()

# Top 10 Products by Profit-----------------
product_profit <- research_data %>%
  group_by(Product_Name) %>%
  summarise(
    Total_Profit = sum(Profit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Profit)) %>%
  head(10)

product_profit

#graph...............
ggplot(product_profit,
       aes(x = reorder(Product_Name, Total_Profit),
           y = Total_Profit)) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Products by Profit",
    x = "Product",
    y = "Profit"
  ) +
theme_minimal()

# Top 10 avg remaining Stock Products--------------------
low_stock <- research_data %>%
  group_by(Product_Name) %>%
  summarise(
    Average_Stock = round(mean(Stock)),
    .groups = "drop"
  ) %>%
  arrange(Average_Stock) %>%
  head(10)

low_stock
#graph............................
ggplot(low_stock,
       aes(x = reorder(Product_Name, Average_Stock),
           y = Average_Stock)) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Low Stock Products",
    x = "Product Name",
    y = "Average Stock"
  ) +
  
  theme_minimal()

# Top 10 Returned Products----------------------
return_product <- research_data %>%
  group_by(Product_Name) %>%
  summarise(
    Total_Return = sum(Return_Count),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Return)) %>%
  head(10)

return_product

#graph.....................
ggplot(return_product,
       aes(x = reorder(Product_Name, Total_Return),
           y = Total_Return)) +
  
  geom_col(fill = "red") +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Returned Products",
    x = "Product Name",
    y = "Return Count"
  ) +
  
  theme_minimal()


#=============================================================
# STEP 16 : Box plot (sales amount,profit) // to find spread,outliers,median
#=============================================================

# 1.sales amount.......................................
ggplot(research_data,
       aes(y = Sales_Amount)) +
  
  geom_boxplot(fill = "steelblue") +
  
  labs(
    title = "Box Plot of Sales Amount",
    y = "Sales Amount"
  ) +
  
  theme_minimal()

# 2. Profit............................................
ggplot(research_data,
       aes(y = Profit)) +
  
  geom_boxplot(fill = "darkgreen") +
  
  labs(
    title = "Box Plot of Profit",
    y = "Profit"
  ) +
  
  theme_minimal()


#=============================================================
# Correlation Heatmap
#=============================================================
numeric_data <- research_data %>%
  select(
    Quantity,
    Selling_Price,
    Discount,
    Shipping_Cost,
    Sales_Amount,
    Cost_Price,
    Stock,
    Return_Count,
    Profit,
    Profit_Margin
  )

cor_matrix <- cor(numeric_data)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",    #upper lower triangle sm 
  tl.cex = 0.8  #txt size
)




#=====================================================
# STEP 17 : Split (80:20)
#=====================================================
set.seed(123)

index <- sample(1:nrow(research_data), 4000)

train_data <- research_data[index, ]

test_data <- research_data[-index, ]

# Check Data Size
dim(train_data)
dim(test_data)


#=========================================================
# STEP 18 : Random Forest Regression
#=========================================================
set.seed(123)   #jeno randomly data or feature na ney

rf_model <- randomForest(
  
  Sales_Amount ~ Quantity +
    Selling_Price +
    Discount +
    Shipping_Cost +
    Year +
    Month+
    Season,
  
  data = train_data,
  
  ntree = 100 # 100ta dscn tree predict kre oitar avg kre result dbe
)

#prediction...............
rf_prediction <- predict(
  rf_model,
  test_data
)
head(rf_prediction)   #6 row result

#evaluation..............
rmse_rf <- sqrt(mean((test_data$Sales_Amount - rf_prediction)^2))  #avg error

mae_rf <- mean(abs(test_data$Sales_Amount - rf_prediction))     #avg absoulate error

r2_rf <- cor(test_data$Sales_Amount,   #actual r prediction ktota milse
             rf_prediction)^2         #range: -1 to +1

#result....................
cat("Random Forest Result\n")

cat("RMSE :", rmse_rf,"\n")

cat("MAE :", mae_rf,"\n")

cat("R Square :", r2_rf,"\n")


#graph........................
ggplot(
  data.frame(
    Actual = test_data$Sales_Amount,
    Predicted = rf_prediction
  ),
  aes(
    x = Actual,
    y = Predicted
  )
) +
  
  geom_point(
    color = "steelblue"  #test data
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red"    #perfect prediction line
  ) +
  
  labs(
    
    title = "Random Forest : Actual vs Predicted",
    
    x = "Actual Sales",
    
    y = "Predicted Sales"
    
  ) +
  
  theme_minimal()

#-----------------------------------------------------------
importance(rf_model) #Which variables are most important for sales prediction
#-----------------------------------------------------------

#=========================================================
# STEP 19 : XGBoost Regression
#=========================================================

set.seed(123)   # jeno same random result reproduce kora jay

#---------------------------------------------------------
# Prepare training and testing data
#---------------------------------------------------------

x_train <- model.matrix(   #only numeric matrix input
  Sales_Amount ~ Quantity +
    Selling_Price +
    Discount +
    Shipping_Cost +
    Year +
    Month +
    Season,
  data = train_data
)[, -1]    #1st intercept column

x_test <- model.matrix(
  Sales_Amount ~ Quantity +
    Selling_Price +
    Discount +
    Shipping_Cost +
    Year +
    Month +
    Season,
  data = test_data
)[, -1]

y_train <- train_data$Sales_Amount
y_test <- test_data$Sales_Amount

#---------------------------------------------------------
# XGBoost Model
#---------------------------------------------------------

xgb_model <- xgboost(
  
  data = x_train,
  
  label = y_train,
  
  nrounds = 300,       # 300ta boosting round(prottekta tree agertar vul thik kre)
  
  max_depth = 6,       # tree er maximum depth
  
  eta = 0.05,          # learning rate(prottekta tree agertar ktotuk vul thik krbe)
  
  subsample = 0.8,     # 80% training data use korbe(overfitting kome)
  
  colsample_bytree = 0.8,  # 80% features use korbe(feature kyta use krbe)
  
  objective = "reg:squarederror",
  
  eval_metric = "rmse",
  
  verbose = 0
)

#---------------------------------------------------------
# Prediction
#---------------------------------------------------------

xgb_prediction <- predict(
  xgb_model,
  x_test
)

head(xgb_prediction)

#---------------------------------------------------------
# Evaluation
#---------------------------------------------------------

rmse_xgb <- sqrt(
  mean(
    (test_data$Sales_Amount - xgb_prediction)^2
  )
)

mae_xgb <- mean(
  abs(
    test_data$Sales_Amount - xgb_prediction
  )
)

# R Square
r2_xgb <- 1 - sum(
  (test_data$Sales_Amount - xgb_prediction)^2
) / sum(
  (test_data$Sales_Amount -
     mean(test_data$Sales_Amount))^2
)

#---------------------------------------------------------
# Result
#---------------------------------------------------------

cat("XGBoost Regression Result\n")

cat("RMSE :", rmse_xgb, "\n")

cat("MAE :", mae_xgb, "\n")

cat("R Square :", r2_xgb, "\n")

#---------------------------------------------------------
# Graph
#---------------------------------------------------------

ggplot(
  data.frame(
    Actual = test_data$Sales_Amount,
    Predicted = xgb_prediction
  ),
  aes(
    x = Actual,
    y = Predicted
  )
) +
  
  geom_point(
    color = "steelblue"
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red"
  ) +
  
  labs(
    title = "XGBoost : Actual vs Predicted",
    x = "Actual Sales",
    y = "Predicted Sales"
  ) +
  
  theme_minimal()

#---------------------------------------------------------
# Feature Importance
#---------------------------------------------------------

xgb_importance <- xgb.importance(
  model = xgb_model
)

xgb_importance


#=========================================================
# STEP 20 : ML Model Performance Comparison
#=========================================================

model_comparison <- data.frame(
  
  Model = c(
    "Random Forest",
    "XGBoost"
  ),
  
  RMSE = c(
    rmse_rf,
    rmse_xgb
  ),
  
  MAE = c(
    mae_rf,
    mae_xgb
  ),
  
  R_Square = c(
    r2_rf,
    r2_xgb
  )
)

model_comparison

# Comparison Graph----------------------------
ggplot(
  model_comparison,
  aes(
    x = Model,
    y = R_Square,
    fill = Model
  )
) +
  
  geom_col() +
  
  labs(
    title = "Comparison of Random Forest and XGBoost",
    x = "Model",
    y = "R Square"
  ) +
  
  theme_minimal()

#=================================================================================
# STEP 21 : K-Means Customer Segmentation
#=================================================================================

# Remove Guest Customer
customer_data <- research_data %>%
  filter(Customer_ID != "Guest Customer") %>%
  group_by(Customer_ID) %>%
  summarise(
    Total_Sales = sum(Sales_Amount, na.rm = TRUE),
    Total_Orders = n(),
    Average_Sales = mean(Sales_Amount, na.rm = TRUE),
    Total_Quantity = sum(Quantity, na.rm = TRUE),
    .groups = "drop"
  )

# Check customer data
head(customer_data)
# Check number of customers
dim(customer_data)

# Select numerical variables
customer_features <- customer_data %>%
  select(
    Total_Sales,
    Total_Orders,
    Average_Sales,
    Total_Quantity
  )

# Scaling(for sm importance of every feature)
customer_scaled <- scale(customer_features)
head(customer_scaled)

#=================================================================================
# K-Means Model
#=================================================================================

set.seed(123)
kmeans_model <- kmeans(
  customer_scaled,
  centers = 3,   #3 customer segment
  nstart = 25    # model multiple starting point thke clauster krbe
)
# Model result
kmeans_model

# Add cluster number to customer data
customer_data$Cluster <- kmeans_model$cluster
# Check result
head(customer_data)
# Number of customers in each cluster
table(customer_data$Cluster)

#=================================================================================
# Customer Segmentation Graph
#=================================================================================
ggplot(
  customer_data,
  aes(
    x = Total_Orders,
    y = Total_Sales,
    color = factor(Cluster)
  )
) +
  
  geom_point(size = 3) +
  
  labs(
    title = "Customer Segmentation using K-Means",
    x = "Total Orders",
    y = "Total Sales",
    color = "Customer Cluster"
  ) +
  
  theme_minimal()

#=================================================================================
# STEP 22 : Elbow Method for K-Means
#=================================================================================

set.seed(123)
wss <- numeric(10)  #within clauster sum of square
for (k in 1:10) {
  
  kmeans_result <- kmeans(
    customer_scaled,
    centers = k,
    nstart = 25
  )
  
  wss[k] <- kmeans_result$tot.withinss
}

# Elbow Graph
plot(
  1:10,
  wss,
  type = "b", #draw both point nd line
  pch = 19,   #shape of point
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares",
  main = "Elbow Method for K-Means"
)

#=================================================================================
# STEP 23 : Customer Cluster Profile
#=================================================================================
cluster_profile <- customer_data %>%
  group_by(Cluster) %>%
  summarise(
    Customers = n(),
    Average_Sales = round(mean(Total_Sales), 2),
    Average_Orders = round(mean(Total_Orders), 2),
    Average_Purchase = round(mean(Average_Sales), 2),
    Average_Quantity = round(mean(Total_Quantity), 2),
    .groups = "drop"
  )
cluster_profile

#=========================================================
# Cluster Profile - Average Sales
#=========================================================
ggplot(
  cluster_profile,
  aes(
    x = factor(Cluster),
    y = Average_Sales,
    fill = factor(Cluster)
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(label = round(Average_Sales)),
    vjust = -0.5,   #text position
    size = 5
  ) +
  
  scale_x_discrete(
    labels = paste0(cluster_profile$Customers, " Customers")
  ) +
  
  labs(
    title = "Average Sales by Customer Cluster",
    x = "Customer Cluster",
    y = "Average Sales",
    fill = "Cluster"
  ) +
  
  theme_minimal()

