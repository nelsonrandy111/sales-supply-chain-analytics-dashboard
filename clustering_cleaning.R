library(tidyverse)
install.packages("fastDummies")
library(fastDummies)
library(lubridate)


df <- read_csv('data/DataCoSupplyChainDataset.csv')

complete_orders <- df %>% 
  filter(`Order Status` %in% c('COMPLETE', 'CLOSED'))

drop_columns <- c('Delivery Status', 'Category Id', 'Customer City', 'Customer Email', 
                  'Customer State', 'Customer Street', 'Customer Zipcode', 'Department Id',
                  'Latitude', 'Longitude', 'Order Item Cardprod Id',
                  'Order Item Product Price', 'Order Region',
                  'Order State', 'Order Status', 'Order Zipcode', 'Product Card Id', 'Product Category Id',
                  'Product Description', 'Product Image', 'Product Name', 'Product Price', 'Product Status',
                  'shipping date (DateOrders)', 'Type', 'Customer Lname', 'Customer Password', 'Customer Fname',
                  'Order Customer Id', 'Order City', 'Order Country', 'Market',
                  'Sales per customer','Order Item Profit Ratio','Order Item Total', 'Category Name', 'Customer Country',
                  'Benefit per order', 'Late_delivery_risk', 'Department Name')

df_cluster <- complete_orders %>% 
  select(-all_of(drop_columns))

df_cluster <- df_cluster %>% 
  mutate(shipping_days_diff = `Days for shipping (real)` - `Days for shipment (scheduled)`)

df_cluster <- df_cluster %>% 
  select(-all_of(c('Days for shipment (scheduled)', 'Days for shipping (real)')))

dummy_columns = c('Customer Segment', 'Shipping Mode')

df_cluster <- dummy_cols(df_cluster, select_columns = dummy_columns, remove_first_dummy = FALSE)


df_cluster$`order date (DateOrders)` <- mdy_hm(df_cluster$`order date (DateOrders)`)
max_date <- max(df_cluster$`order date (DateOrders)`, na.rm = TRUE)

df_cluster <- df_cluster %>%
  group_by(`Customer Id`) %>%
  mutate(
    MostRecentOrder = max(`order date (DateOrders)`, na.rm = TRUE),
    FirstOrder = min(`order date (DateOrders)`, na.rm = TRUE),
    Recency = as.numeric(difftime(max_date, MostRecentOrder, units = "days")),
    Tenure = as.numeric(difftime(max_date, FirstOrder, units = "days"))
  ) %>%
  ungroup()

binary_vars <- df_cluster %>%
  select(where(~ is.numeric(.x) && all(na.omit(unique(.x)) %in% c(0, 1)))) %>%
  names()
print(binary_vars)

# Calculate % of orders per department per customer
categorical_percentages <- df_cluster %>%
  group_by(`Customer Id`) %>%
  summarise(across(all_of(binary_vars), ~ mean(.x), .names = "Pct_{.col}")) %>% 
  ungroup()

df_cluster <- df_cluster %>%
  left_join(categorical_percentages, by = "Customer Id")

df_cluster <- df_cluster %>% 
  select(-all_of(binary_vars))

df_cluster <- df_cluster %>% 
  group_by(`Customer Id`) %>% 
  mutate(NumberOfOrders = n_distinct(`Order Id`)) %>% 
  ungroup()

drop <- c("Customer Segment", "order date (DateOrders)" ,
          "Order Id", "Order Item Id", "Shipping Mode", "MostRecentOrder", "FirstOrder",
          "Pct_Customer Segment_Corporate")

df_cluster <- df_cluster %>% 
  select(-all_of(drop))

customer_summary <- df_cluster %>% 
  group_by(`Customer Id`) %>% 
  summarise(
    TotalDiscount = sum(`Order Item Discount`, na.rm = TRUE),
    AvgDiscount = mean(`Order Item Discount Rate`, na.rm = TRUE),
    TotalQuantity = sum(`Order Item Quantity`, na.rm = TRUE),
    TotalSales = sum(Sales, na.rm = TRUE),
    TotalProfit = sum(`Order Profit Per Order`, na.rm = TRUE),
    AvgShippingError = mean(shipping_days_diff, na.rm = TRUE),
    Recency = max(Recency, na.rm = TRUE),
    Tenure = max(Tenure, na.rm = TRUE),
    IsConsumer = max(`Pct_Customer Segment_Consumer`),
    across(
      starts_with("Pct_Shipping Mode_"),
      ~ max(.x, na.rm = TRUE),
      .names = "PctShippingMode{str_remove(.col, 'Pct_Shipping Mode_')}"), 
    TotalNumberOfOrders = sum(NumberOfOrders)
  )


customer_summary <- customer_summary %>% 
  mutate(AverageDiscount = (TotalDiscount/TotalSales),
         AverageOrderSales = (TotalSales/TotalNumberOfOrders),
         AverageOrderQuantity = (TotalQuantity/TotalNumberOfOrders),
         AverageOrderProfit = (TotalProfit/TotalNumberOfOrders)
  )

customer_summary_drop <- c('Customer Id', 'TotalDiscount', 'AvgDiscount',
                           'TotalQuantity', 'TotalSales', 'TotalProfit')

clustering_data <- customer_summary %>% 
  select(-all_of(customer_summary_drop))

write.csv(clustering_data, file='data/clustering_data.csv', row.names = FALSE)

