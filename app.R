# ----------------------- LOAD LIBRARIES ---------------------------- #
library(shiny)
library(bslib)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(leaflet.extras)
library(tidyr)
library(cluster)
library(factoextra)
library(isotree)
library(scales)
library(tidyverse)

# ------------------------------------------ LOAD THE DATA & DEFINE VARIABLES ----------------------------------------------- #
# Load and clean data
data <- read.csv("data/DataCoSupplyChainDataset.csv") # Main sales data 
cluster <- read.csv("data/clustering_data.csv") # Cluster cleaned data  

#Define a vector with the features to scale the model
scale_features <- c("AvgShippingError", "Recency", "Tenure","TotalNumberOfOrders", "AverageDiscount",
                    "AverageOrderSales", "AverageOrderQuantity","AverageOrderProfit") 

# ------------------------------------------ CLEAN AND PREPROCESS THE DATA ----------------------------------------------- #

# Transform and clean column names
names(data) <- trimws(names(data))
names(data) <- gsub(" ", ".", names(data))
names(data) <- make.names(names(data), unique = TRUE)  # Normalize names

# Rename the specific date columns 
data$Order.Date <- data$order.date..DateOrders.
data$Order.Date <- parse_date_time(data$Order.Date, orders = "mdy HM")
data$Order.Date <- as.Date(data$Order.Date)
data$Ship.Date <- data$shipping.date..DateOrders.
data$Ship.Date <- parse_date_time(data$Ship.Date, orders = "mdy HM")
data$Ship.Date <- as.Date(data$Ship.Date)

# Add month column
data$Month <- format(data$Order.Date, "%Y-%m")

# Data Cleanup & Feature Engineering
data <- data %>%
  distinct() %>%
  filter(!is.na(Order.Date), !is.na(Sales), !is.na(Order.Profit.Per.Order)) %>%
  filter(nchar(Customer.State) == 2) %>% 
  mutate(
    Order.Status = toupper(Order.Status),
    Delivery.Delay = as.numeric(Ship.Date - Order.Date),
    Customer.Lname = trimws(Customer.Lname), # Check for the name (some names are not correct or have strange format)
    Customer.City = trimws(Customer.City))

# ------------------------------------------ DEFINE FUNCTIONS TO USE IN THE DASHBOARD FOR REUSABILITY ----------------------------------------------- #

# Currency function
format_currency <- function(x) paste0("$", format(round(x, 2), big.mark = ","))

# State function to transform the data
state_name <- function(state_code) {
  state_df <- data.frame(
    code = c(state.abb, "DC", "PR"),
    name = c(state.name, "Washington, D.C.", "Puerto Rico"))
  return(state_df$name[match(state_code, state_df$code)])}

# Generate PCA plot for clusters 
generate_pca_plot <- function(pca_df) {
  centers <- pca_df %>%
    group_by(cluster) %>%
    summarise(across(c(PC1, PC2), mean), .groups = "drop")
  
  ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
    geom_jitter(alpha = 0.5, size = 1.2) +
    stat_ellipse(type = "norm", level = 0.95, linetype = "dashed") +
    geom_point(data = centers, aes(x = PC1, y = PC2),
               shape = 4, size = 5, stroke = 2, color = "black") +
    scale_color_brewer(palette = "Set2") +
    labs(title = "PCA Plot with Clusters & Centroids",
         x = "PC1", y = "PC2") +
    theme_minimal(base_size = 15)}

# ------------------------------------------ MAIN UI PAGE ----------------------------------------------- #
# Define the UI and the tabs 
ui <- page_navbar(title = "Sales & Supplier Dashboard",
                  
                  # --------- Executive Summary Page ---------- #
                  nav_panel(
                    "Executive Summary",
                    icon = icon("tachometer-alt"),
                    page_fluid(
                      # Main KPIs Boxes
                      layout_column_wrap(width = "20rem",
                                         value_box(
                                           title = "Total Sales",
                                           value = textOutput("total_sales"),
                                           showcase = icon("piggy-bank"),
                                           theme = "green"
                                         ),
                                         value_box(
                                           title = "Total Profit",
                                           value = textOutput("total_profit"),
                                           showcase = icon("chart-line"),
                                           theme = "cyan"
                                         ),
                                         value_box(
                                           title = "Total Orders",
                                           value = textOutput("total_orders"),
                                           showcase = icon("boxes"),
                                           theme = "blue"
                                         ),
                                         value_box(
                                           title = "Customers",
                                           value = textOutput("total_customers"),
                                           showcase = icon("users"),
                                           theme = "purple"
                                         )
                      ),
                      
                      # Year Filter
                      selectInput(
                        inputId = "year_filter",
                        label = "Select Year:",
                        choices = c("All", sort(unique(format(data$Order.Date, "%Y")))),
                        selected = "All",
                        width = "200px"
                      ),
                      
                      # Top 5 tables
                      layout_column_wrap(width = "30rem",
                                         card(card_header("Top 5 Departments by Sales"), tableOutput("top_products")),
                                         card(card_header("Top 5 States & by Sales"), tableOutput("top_states"))
                      ),
                      # Sales trend plot
                      card(
                        card_header("Sales Comparison: Actual Month vs Previuos Month"),
                        plotOutput("exec_sales_plot")
                      )
                    )
                  ),
                  
                  # --------- Product Analysis Page ---------- #
                  nav_panel(
                    "Product Analysis",
                    icon = icon("box"),
                    page_fluid(
                      # Year and Month Filters
                      layout_columns(
                        width = c("200px", "200px"),  # Two fixed-width columns
                        selectInput(
                          "product_year_filter",
                          "Select Year:",
                          choices = c("All", sort(unique(format(data$Order.Date, "%Y")))),
                          selected = "All"
                        ),
                        selectInput(
                          "product_month_filter",
                          "Select Month:",
                          choices = c("All", month.name),
                          selected = "All"
                        )
                      ),
                      layout_column_wrap(
                        width = "30rem",
                        card(
                          card_header("Top 10 Products by Sales"),
                          plotOutput("top_products_plot")
                        ),
                        card(
                          card_header("Revenue by Product Category"),
                          plotOutput("category_sales_plot")
                        )
                      ),
                      card(
                        card_header("Product Table"),
                        DT::dataTableOutput("product_table")
                      )
                    )
                  ),
                  
                  # --------- Orders Page ---------- #
                  nav_panel(
                    "Orders",
                    icon = icon("truck"),
                    page_fluid(
                      page_sidebar(
                        sidebar = sidebar(
                          width = "20rem",
                          selectizeInput(
                            "orders_order_id",
                            "Order ID",
                            choices = character(0),  # Avoids warning
                            selected = "All",
                            multiple = FALSE,
                            options = list(placeholder = "Select an order...", maxOptions = 10)
                          ),
                          dateRangeInput(
                            "orders_date_range",
                            "Date Range",
                            start = min(data$Order.Date, na.rm = TRUE),
                            end = max(data$Order.Date, na.rm = TRUE)
                          ),
                        ),
                        page_fillable(
                          layout_column_wrap(
                            width = "100%",
                            card(
                              card_header("Geographic Distribution of Orders by Sales Volume"),
                              leafletOutput("supplier_map", height = "500px")
                            )
                          ),
                          layout_column_wrap(
                            width = "100%",
                            card(
                              card_header("Filtered Orders Table"),
                              DT::dataTableOutput("supplier_orders_table")
                            )
                          )
                        )
                      )
                    )
                  ),
                  # --------- Sales Overview Page ---------- #
                  nav_panel(
                    "Sales Overview",
                    icon = icon("chart-bar"),
                    page_fluid(
                      page_sidebar(
                        sidebar = sidebar(
                          width = "18rem",
                          dateRangeInput(
                            "sales_date_range",
                            "Filter by Order Date",
                            start = min(data$Order.Date, na.rm = TRUE),
                            end = max(data$Order.Date, na.rm = TRUE)
                          ),
                          selectInput(
                            "country_filter",
                            "Select Country",
                            choices = c("All", sort(unique(data$Customer.Country))),
                            selected = "All"
                          ),
                          selectInput(
                            "segment_filter",
                            "Customer Segment",
                            choices = c("All", "Business", "Consumer"),
                            selected = "All"
                          ),
                          selectInput(
                            "department_filter",
                            "Product Department",
                            choices = c("All", sort(unique(data$Department.Name))),
                            selected = "All"
                          )
                        ),
                        page_fillable(
                          layout_column_wrap(
                            width = "100%",
                            card(
                              card_header("Profit vs Average Discount Over Time"),
                              plotOutput("discount_profit_plot", height = "400px")
                            )
                          ),
                          layout_column_wrap(
                            width = "100%",
                            card(
                              card_header("Sales by Customer Segment"),
                              plotOutput("sales_by_segment", height = "400px")
                            )
                          ),
                          layout_column_wrap(
                            width = "100%",
                            card(
                              card_header("Monthly KPIs"),
                              DT::dataTableOutput("monthly_kpis")
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  # --------- Segmentation Page ---------- #
                  nav_panel(
                    "Segmentation",
                    icon = icon("project-diagram"),
                    navset_tab(
                      # Customer Clusters Tab
                      nav_panel(
                        "Customer Clusters",
                        page_sidebar(
                          sidebar = sidebar(
                            sliderInput("k_cons", "Select Number of Clusters", min = 2, max = 8, value = 4),
                            downloadButton("download_plot_cons", "Download Cluster Plot (PNG)"),
                            downloadButton("download_table_cons", "Download Table (CSV)"),
                            tags$hr(),
                            tags$h5(icon("question-circle"), "What are these plots?"),
                            tags$p(
                              icon("chart-line"), strong("Elbow Method:"), 
                              "Helps find the optimal number of clusters (k) by showing when adding more clusters doesn't significantly reduce error."
                            ),
                            tags$p(
                              icon("project-diagram"), strong("Silhouette Score:"), 
                              "Measures how similar a point is to its own cluster vs others. Higher score means better defined clusters."
                            )
                          ),
                          layout_column_wrap(
                            width = "30rem",
                            card(card_header("Elbow Method"), plotOutput("elbow_plot_cons", height = "250px")),
                            card(card_header("Silhouette Score"), plotOutput("sil_plot_cons", height = "250px"))
                          ),
                          card(card_header("PCA Cluster Plot"), plotOutput("pca_plot_cons", height = "500px")),
                          card(card_header("Customer Cluster Summary"), DTOutput("table_cons"))
                        )
                      ),
                      
                      # Business Clusters Tab 
                      nav_panel(
                        "Business Clusters",
                        page_sidebar(
                          sidebar = sidebar(
                            sliderInput("k_bus", "Select Number of Clusters", min = 2, max = 8, value = 4),
                            downloadButton("download_plot_bus", "Download Cluster Plot (PNG)"),
                            downloadButton("download_table_bus", "Download Table (CSV)"),
                            tags$hr(),
                            tags$h5(icon("question-circle"), "What are these plots?"),
                            tags$p(
                              icon("chart-line"), strong("Elbow Method:"), 
                              "Helps find the optimal number of clusters (k) by showing when adding more clusters doesn't significantly reduce error."
                            ),
                            tags$p(
                              icon("project-diagram"), strong("Silhouette Score:"), 
                              "Measures how similar a point is to its own cluster vs others. Higher score means better defined clusters."
                            )
                          ),
                          layout_column_wrap(
                            width = "30rem",
                            card(card_header("Elbow Method"), plotOutput("elbow_plot_bus", height = "250px")),
                            card(card_header("Silhouette Score"), plotOutput("sil_plot_bus", height = "250px"))
                          ),
                          card(card_header("PCA Cluster Plot"), plotOutput("pca_plot_bus", height = "500px")),
                          card(card_header("Business Cluster Summary"), DTOutput("table_bus"))
                        )
                      )
                    )
                  ),
                  nav_spacer(),
                  nav_item(
                    input_dark_mode(),
                  ),
)

# ------------------------------------------ SERVER FEATURES -----------------------------------------------
server <- function(input, output, session) {
  # ------------ Filters for the pages -------------
  
  observe({
    updateSelectizeInput(
      session,
      "orders_order_id",
      choices = c("All", sort(unique(data$Order.Id))),
      server = TRUE)})
  
  # compute the cluster ones here
  # For customer
  km_cons_model <- reactive({
    kmeans(cons_pca_data, centers = input$k_cons, nstart = 15, iter.max = 100)
  })
  # For business
  km_bus_model <- reactive({
    kmeans(bus_pca_data, centers = input$k_bus, nstart = 15, iter.max = 100)
  })
  
  # Filter for the years in the executive summary
  filtered_year_data <- reactive({
    req(input$year_filter)
    if (input$year_filter == "All") {
      data} 
    else {data %>% filter(format(Order.Date, "%Y") == input$year_filter)}})
  
  # Filter for the years and months in the executive summary
  filtered_product_data <- reactive({
    df <- data
    if (input$product_year_filter != "All") {
      df <- df %>% filter(format(Order.Date, "%Y") == input$product_year_filter)}
    if (input$product_month_filter != "All") {
      df <- df %>% filter(format(Order.Date, "%B") == input$product_month_filter)}
    df})
  
  # Filters for the supplier
  suppliers_filtered <- reactive({
    req(input$orders_order_id)
    req(input$orders_date_range)
    df <- data %>%
      filter(
        Order.Date >= input$orders_date_range[1],
        Order.Date <= input$orders_date_range[2])
    if (input$orders_order_id != "All") {
      df <- df %>% filter(Order.Id == input$orders_order_id)}
    df})
  
  
  # Filters for the sales page
  filtered_sales <- reactive({
    req(input$sales_date_range)
    data %>%
      mutate(Segment = case_when(
        Customer.Segment %in% c("Corporate", "Home Office") ~ "Business",
        TRUE ~ "Consumer")) %>%
      filter(
        Order.Date >= input$sales_date_range[1],
        Order.Date <= input$sales_date_range[2],
        if (input$country_filter != "All") Customer.Country == input$country_filter else TRUE,
        if (input$segment_filter != "All") Segment == input$segment_filter else TRUE,
        if (input$department_filter != "All") Department.Name == input$department_filter else TRUE)
  })
  
  # ------------ Executive summary Outputs -------------
  # Boxes output, based on the filtered year 
  output$total_sales <- renderText({
    format_currency(sum(filtered_year_data()$Sales, na.rm = TRUE))})
  output$total_profit <- renderText({
    format_currency(sum(filtered_year_data()$Order.Profit.Per.Order, na.rm = TRUE))})
  output$total_orders <- renderText({
    format(n_distinct(filtered_year_data()$Order.Id), big.mark = ",")})
  output$total_customers <- renderText({
    format(n_distinct(filtered_year_data()$Customer.Id), big.mark = ",")})
  
  # Show the top 5 products
  output$top_products <- renderTable({
    filtered_year_data() %>%
      group_by(Department.Name) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(TotalSales)) %>%
      mutate(TotalSales = format_currency(TotalSales)) %>%
      head(5)}, striped = TRUE, bordered = TRUE)
  
  # Show the top 5 states
  output$top_states <- renderTable({
    df <- filtered_year_data()
    if (nrow(df) == 0) {
      return(data.frame(Customer.State = character(), TotalSales = character()))}
    df %>%
      group_by(Customer.State) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalSales)) %>%
      mutate(Customer.State = sapply(Customer.State, state_name)) %>%
      mutate(TotalSales = format_currency(TotalSales)) %>%
      head(5)}, striped = TRUE, bordered = TRUE)
  
  # Show the current monthly trend vs last month (Actual month vs last month)
  output$exec_sales_plot <- renderPlot({
    last_date <- max(data$Order.Date, na.rm = TRUE)
    last_month <- floor_date(last_date, "month")
    prev_month <- last_month %m-% months(1)
    
    # Filter and aggregate sales
    filtered <- data %>%
      filter(floor_date(Order.Date, "month") %in% c(prev_month, last_month)) %>%
      mutate(
        Month = format(Order.Date, "%B"),
        Year = year(Order.Date),
        Day = day(Order.Date)
      ) %>%
      group_by(Month, Year, Day) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE), .groups = "drop")
    
    # Monthly totals for comparison
    totals <- data %>%
      mutate(Month = floor_date(Order.Date, "month")) %>%
      filter(Month %in% c(prev_month, last_month)) %>%
      group_by(Month) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE), .groups = "drop")
    sales_change <- round((totals$Sales[totals$Month == last_month] -
                             totals$Sales[totals$Month == prev_month]) /
                            totals$Sales[totals$Month == prev_month] * 100, 2)
    trend_text <- if (sales_change > 0) {
      paste0("↑ ", sales_change, "% more than ", format(prev_month, "%B %Y"))} 
    else if (sales_change < 0) {
      paste0("↓ ", abs(sales_change), "% less than ", format(prev_month, "%B %Y"))} 
    else {"No change from previous month"}
    # Title of the plot + right legend
    title_text <- paste0(
      "Sales Comparison: ", format(prev_month, "%B %Y"), " vs ", format(last_month, "%B %Y"),
      "\n", trend_text)
    
    filtered <- filtered %>%
      mutate(Month_Year = paste(Month, Year))
    ggplot(filtered, aes(x = Day, y = Sales, color = Month_Year, group = Month_Year)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(
        title = title_text,
        x = "Day of Month", y = "Sales ($)") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))})
  
  # ------------ Product Analysis Outputs -------------
  # Top 10 products plot
  output$top_products_plot <- renderPlot({
    top_products <- filtered_product_data() %>%
      group_by(Product.Name) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(Sales)) %>%
      slice_head(n = 10)
    leader <- top_products$Product.Name[1]
    leader_pct <- round(top_products$Sales[1] / sum(top_products$Sales) * 100, 1)
    ggplot(top_products, aes(x = reorder(Product.Name, Sales), y = Sales)) +
      geom_col(fill = "skyblue") +
      coord_flip() +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(
        title = paste("Top 10 Products by Sales\n", leader, "leads with", leader_pct, "% of Top 10 sales"),
        x = "Product",
        y = "Sales ($)") +
      theme_minimal()})
  
  # Top 10 categories plot 
  output$category_sales_plot <- renderPlot({
    category_sales <- filtered_product_data() %>%
      group_by(Category.Name) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(Sales)) %>%
      slice_head(n = 10)
    leader <- category_sales$Category.Name[1]
    leader_pct <- round(category_sales$Sales[1] / sum(category_sales$Sales) * 100, 1)
    ggplot(category_sales, aes(x = reorder(Category.Name, Sales), y = Sales, fill = Category.Name)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_brewer(palette = "Set3") +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(
        title = paste("Top 10 Product Categories by Sales\n", leader, "leads with", leader_pct, "% of Top 10 sales"),
        x = "Category",
        y = "Sales ($)") +
      theme_minimal()})
  
  # All products Cumulative sells and data
  output$product_table <- DT::renderDataTable({
    filtered_product_data() %>%
      group_by(Product.Name) %>%
      summarise(
        Total.Sales = sum(Sales, na.rm = TRUE),
        Orders = n(),
        Avg.Price = mean(Order.Item.Product.Price, na.rm = TRUE)
      ) %>%
      mutate(
        Total.Sales = format_currency(Total.Sales),
        Avg.Price = format_currency(Avg.Price)
      ) %>%
      arrange(desc(Total.Sales))
  }, options = list(
    pageLength = 25,        # Show 25 rows per page
    lengthMenu = c(10, 25, 50, 100),  # Dropdown menu for user to pick
    scrollX = TRUE
  ))
  
  
  
  # ------------ Orders -------------
  
  # Supplier output map
  output$supplier_map <- renderLeaflet({
    df <- suppliers_filtered()
    
    map_data <- df %>%
      filter(!is.na(Latitude) & !is.na(Longitude)) %>%
      group_by(Customer.City, Customer.State, Latitude, Longitude) %>%
      summarise(
        TotalSales = sum(Sales, na.rm = TRUE),
        Orders = n_distinct(Order.Id),
        .groups = "drop"
      )
    
    # Normalize TotalSales for heatmap intensity
    scale_max <- quantile(map_data$TotalSales, 0.45, na.rm = TRUE)
    map_data$Weight <- pmin(map_data$TotalSales / scale_max, 1)
    # Create a palette function for the legend only
    pal_fun <- colorNumeric(palette = "YlGnBu", domain = map_data$TotalSales)
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addHeatmap(
        lng = ~Longitude,
        lat = ~Latitude,
        intensity = ~Weight,
        radius = 15,
        blur = 2,
        max = 1,
        gradient = "YlGnBu"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_fun,
        values = ~TotalSales,
        title = "Total Sales"
      )
  })
  
  # Order table 
  output$supplier_orders_table <- DT::renderDataTable({
    suppliers_filtered() %>%
      select(
        Order.Id,
        Customer.Id,
        Product.Name,
        Sales,
        Order.Date,
        Ship.Date,
        Customer.City,
        Customer.State
      ) %>%
      arrange(desc(Order.Date))
  }, options = list(
    pageLength = 25,
    lengthMenu = c(10, 25, 50, 100),
    scrollX = TRUE
  ))
  
  # ------------ Sales overview -------------
  output$discount_profit_plot <- renderPlot({
    df <- filtered_sales() %>%
      group_by(Date = as.Date(Order.Date)) %>%
      summarise(
        Profit = sum(Order.Profit.Per.Order, na.rm = TRUE),
        Avg_Discount_Rate = mean(Order.Item.Discount.Rate, na.rm = TRUE),
        .groups = "drop")
    
    # Calculate scaling factor to align Avg Discount with Profit visually
    max_profit <- max(df$Profit, na.rm = TRUE)
    max_discount <- max(df$Avg_Discount_Rate, na.rm = TRUE) * 100
    scale_factor <- max_profit / max_discount
    
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = Profit, color = "Profit ($)"), linewidth = 1.4) +
      geom_line(aes(y = Avg_Discount_Rate * 100 * scale_factor, color = "Avg Discount (%)"),
                linewidth = 1.2, linetype = "dashed") +
      scale_y_continuous(
        name = "Profit ($)",
        labels = dollar_format(),
        sec.axis = sec_axis(~ . / scale_factor, name = "Avg Discount (%)", labels = label_number(suffix = "%"))) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
      scale_color_manual(
        values = c("Profit ($)" = "#e74c3c", "Avg Discount (%)" = "#3498db")) +
      labs(
        x = "Date", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")})
  
  # Sales by Customer Segment
  output$sales_by_segment <- renderPlot({
    df <- filtered_sales() %>%
      mutate(Segment = ifelse(Customer.Segment %in% c("Corporate", "Home Office"), "Business", "Consumer"))
    date_range <- as.numeric(difftime(max(df$Order.Date), min(df$Order.Date), units = "days"))
    
    df <- df %>%
      mutate(Period = case_when(
        date_range < 30 ~ as.Date(Order.Date),
        date_range < 90 ~ floor_date(Order.Date, "week"),
        TRUE ~ floor_date(Order.Date, "month")
      )) %>%
      group_by(Period, Segment) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      group_by(Period) %>%
      mutate(Share = Sales / sum(Sales)) %>%
      ungroup()
    
    # Format the X Axis is was giving a lot of problems
    date_format <- if (date_range < 30) {"%d %b"} 
    else if (date_range < 90) {"%d %b"} 
    else {"%b %Y"}
    date_breaks <- if (date_range < 30) {"5 days"} 
    else if (date_range < 90) {"1 week"} else {"2 months"}
    
    # Plot the lines for each segment 
    ggplot(df, aes(x = Period, y = Sales, color = Segment)) +
      geom_line(size = 1.1) +
      geom_text(
        aes(label = paste0(round(Share * 100), "%")),
        size = 3, vjust = -0.5, show.legend = FALSE, check_overlap = TRUE) +
      scale_color_manual(values = c("Business" = "#e74c3c", "Consumer" = "#1abc9c")) +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_date(date_labels = date_format, date_breaks = date_breaks) +
      labs(title = NULL, x = NULL, y = "Sales ($)") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))})
  
  # Monthly KPIs Table
  output$monthly_kpis <- DT::renderDataTable({
    filtered_sales() %>%
      mutate(Month = floor_date(Order.Date, "month")) %>%
      group_by(Month) %>%
      summarise(
        Sales = sum(Sales, na.rm = TRUE),
        Profit = sum(Order.Profit.Per.Order, na.rm = TRUE),
        Orders = n_distinct(Order.Id)) %>%
      mutate(
        Sales = dollar(Sales),
        Profit = dollar(Profit),
        Orders = format(Orders, big.mark = ","))})
  
  # -------- CONSUMER CLUSTERS MODEL -------- #
  # Prepare the data for the model
  consumer_data <- cluster %>% filter(IsConsumer == 1) %>%
    select(-starts_with("PctDepartment"), -IsConsumer)
  cons_scaled <- consumer_data %>% mutate(across(all_of(scale_features), scale))
  cons_pca <- prcomp(cons_scaled, scale. = FALSE)
  cons_pca_data <- as.data.frame(cons_pca$x[, 1:5])
  
  # Run the evaluation metrics 
  elbow_cons <- tibble(k = 1:8) %>%
    mutate(km = map(k, ~ kmeans(cons_pca_data, .x, nstart = 15, iter.max = 100)),
           tot_withinss = map_dbl(km, ~ .x$tot.withinss))
  
  sil_cons <- tibble(k = 2:8) %>%
    mutate(km = map(k, ~ kmeans(cons_pca_data, .x, nstart = 15, iter.max = 100)),
           avg_sil_width = map_dbl(km, ~ mean(silhouette(.x$cluster, dist(cons_pca_data))[, 3])))
  
  # Plot the elbow method
  output$elbow_plot_cons <- renderPlot({
    title_text <- "Optimal K(clusters) for K-Means clustering"
    ggplot(elbow_cons, aes(x = k, y = tot_withinss)) +
      geom_line() + geom_point() +
      geom_vline(xintercept = input$k_cons, linetype = "dashed", color = "red") +
      labs(title = title_text, x = "K", y = "Total Within SS") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))})
  
  # Plot the Silhouette Score
  output$sil_plot_cons <- renderPlot({
    title_text <- "Clustering quality for different values of K(clusters)"
    ggplot(sil_cons, aes(x = k, y = avg_sil_width)) +
      geom_line() + geom_point() +
      geom_vline(xintercept = input$k_cons, linetype = "dashed", color = "red") +
      labs(title = title_text, x = "K", y = "Average Width") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))})
  
  # Run the PCA into the model
  pca_cons_clusters <- reactive({
    km <- km_cons_model()
    as.data.frame(cons_pca$x[, 1:2]) %>%
      mutate(cluster = as.factor(km$cluster))})
  
  # Run the PCA plot
  output$pca_plot_cons <- renderPlot({
    title_text <- paste0("PCA Plot with Clusters & Centroids\nFinal K-Means result for K = ", input$k_cons)
    generate_pca_plot(pca_cons_clusters()) +
      ggtitle(title_text) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))})
  
  # A table to show the characteristics of cluster 
  output$table_cons <- renderDT({
    km <- km_cons_model()
    consumer_data %>%
      mutate(cluster = as.factor(km$cluster)) %>%
      group_by(cluster) %>%
      summarise(across(where(is.numeric), ~ round(mean(.x), 2)),
                n_customers = n())})
  
  # Add downloading options
  output$download_plot_cons <- downloadHandler(
    filename = function() paste0("consumer_pca_k", input$k_cons, ".png"),
    content = function(file) {
      png(file, width = 1200, height = 1000)
      print(generate_pca_plot(pca_cons_clusters()))
      dev.off()})
  
  # Add downloading options
  output$download_table_cons <- downloadHandler(
    filename = function() paste0("consumer_table_k", input$k_cons, ".csv"),
    content = function(file) {
      km <- km_cons_model()
      write_csv(
        consumer_data %>%
          mutate(cluster = as.factor(km$cluster)) %>%
          group_by(cluster) %>%
          summarise(across(where(is.numeric), ~ round(mean(.x), 2)),
                    n_customers = n()),file)})
  
  ## -------- BUSINESS CLUSTERS -------- ##
  
  # Prepare the data for the model
  business_data <- cluster %>% filter(IsConsumer == 0) %>% select(-IsConsumer)
  bus_scaled <- business_data %>% mutate(across(all_of(scale_features), scale))
  iso_input <- bus_scaled %>% select(all_of(scale_features)) %>% as.matrix()
  iso_model <- isolation.forest(iso_input, ntrees = 200)
  scores <- predict(iso_model, iso_input, type = "score")
  business_data <- business_data[scores < 0.5, ]
  bus_scaled <- bus_scaled[scores < 0.5, ]
  bus_pca <- prcomp(bus_scaled, scale. = FALSE)
  bus_pca_data <- as.data.frame(bus_pca$x[, 1:5])
  
  # Run the evaluation metrics 
  elbow_bus <- tibble(k = 1:8) %>%
    mutate(km = map(k, ~ kmeans(bus_pca_data, .x, nstart = 15, iter.max = 100)),
           tot_withinss = map_dbl(km, ~ .x$tot.withinss))
  
  # Plot the Silhouette Score
  sil_bus <- tibble(k = 2:8) %>%
    mutate(km = map(k, ~ kmeans(bus_pca_data, .x, nstart = 15, iter.max = 100)),
           avg_sil_width = map_dbl(km, ~ mean(silhouette(.x$cluster, dist(bus_pca_data))[, 3])))
  
  # Plot the Silhouette Score
  output$elbow_plot_bus <- renderPlot({
    title_text <- "Optimal K(clusters) for K-Means clustering"
    ggplot(elbow_bus, aes(x = k, y = tot_withinss)) +
      geom_line() + geom_point() +
      geom_vline(xintercept = input$k_bus, linetype = "dashed", color = "red") +
      labs(title = title_text, x = "K", y = "Total Within SS") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))})
  
  output$sil_plot_bus <- renderPlot({
    title_text <- "Clustering quality for different values of K(clusters)"
    ggplot(sil_bus, aes(x = k, y = avg_sil_width)) +
      geom_line() + geom_point() +
      geom_vline(xintercept = input$k_bus, linetype = "dashed", color = "red") +
      labs(title = title_text, x = "K", y = "Average Width") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))})
  
  # Run the PCA into the model
  pca_bus_clusters <- reactive({
    km <- km_bus_model()
    as.data.frame(bus_pca$x[, 1:2]) %>%
      mutate(cluster = as.factor(km$cluster))})
  
  # Run the PCA plot
  output$pca_plot_bus <- renderPlot({
    title_text <- paste0("PCA Plot with Clusters & Centroids\nFinal K-Means result for K = ", input$k_bus)
    generate_pca_plot(pca_bus_clusters()) +
      ggtitle(title_text) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))})
  
  # A table to show the characteristics of cluster 
  output$table_bus <- renderDT({
    km <- km_bus_model()
    business_data %>%
      mutate(cluster = as.factor(km$cluster)) %>%
      group_by(cluster) %>%
      summarise(across(where(is.numeric), ~ round(mean(.x), 2)),
                n_customers = n())})
  
  # Add downloading options
  output$download_plot_bus <- downloadHandler(
    filename = function() paste0("business_pca_k", input$k_bus, ".png"),
    content = function(file) {
      png(file, width = 1200, height = 1000)
      print(generate_pca_plot(pca_bus_clusters()))
      dev.off()})
  
  # Add downloading options
  output$download_table_bus <- downloadHandler(
    filename = function() paste0("business_table_k", input$k_bus, ".csv"),
    content = function(file) {
      km <- km_bus_model()
      write_csv(
        business_data %>%
          mutate(cluster = as.factor(km$cluster)) %>%
          group_by(cluster) %>%
          summarise(across(where(is.numeric), ~ round(mean(.x), 2)),
                    n_customers = n()),file)})}
# run app
shinyApp(ui,server)