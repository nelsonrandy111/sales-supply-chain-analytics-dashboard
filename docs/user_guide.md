# User Guide - Sales & Supply Chain Analytics Dashboard

This guide will help you navigate and effectively use the Sales & Supply Chain Analytics Dashboard.

## Getting Started

### First Time Setup
1. **Install R and RStudio** (if not already installed)
2. **Ensure data files are in the `data/` directory**
3. **Run the installation script**: `source("install_packages.R")`
4. **Start the dashboard**: Open `app.R` and click "Run App"
5. **Access the dashboard**: Open your web browser to the provided URL

### Quick Start
```r
# In R console
source("install_packages.R")
shiny::runApp()
```

## Dashboard Overview

The dashboard consists of 5 main sections:

### 1. Executive Summary
**Purpose**: High-level business overview and KPIs

**Key Features**:
- **Real-time KPIs**: Total sales, profit, orders, and customers
- **Year Filter**: Analyze performance by specific years
- **Top Performers**: Best departments and states by sales
- **Trend Analysis**: Month-over-month sales comparison

**How to Use**:
- Select a year from the dropdown to filter data
- View the top 5 departments and states tables
- Analyze the sales trend chart for insights

### 2. Product Analysis
**Purpose**: Deep dive into product performance and categories

**Key Features**:
- **Top Products**: 10 best-performing products by sales
- **Category Analysis**: Revenue breakdown by product category
- **Interactive Tables**: Detailed product information
- **Time Filtering**: Filter by year and month

**How to Use**:
- Use year and month filters to focus on specific periods
- Analyze the bar charts for product and category performance
- Use the interactive table to drill down into specific products
- Export data for further analysis

### 3. Orders & Geographic Analysis
**Purpose**: Geographic distribution and order tracking

**Key Features**:
- **Interactive Map**: Heatmap showing sales by location
- **Order Filtering**: Filter by date range and order ID
- **Geographic Insights**: Customer location analysis
- **Order Details**: Comprehensive order information table

**How to Use**:
- Use the sidebar filters to narrow down orders
- Click on map areas to see sales intensity
- Analyze the order table for detailed information
- Export filtered data for reporting

### 4. Sales Overview
**Purpose**: Advanced sales analysis and customer insights

**Key Features**:
- **Profit vs Discount Analysis**: Correlation between pricing and profitability
- **Customer Segment Analysis**: Business vs Consumer performance
- **Monthly KPIs**: Detailed monthly performance metrics
- **Advanced Filtering**: Multiple filter options

**How to Use**:
- Set date ranges for analysis periods
- Filter by country, customer segment, and department
- Analyze the dual-axis chart for profit/discount insights
- Review monthly KPIs for trend identification

### 5. Customer Segmentation
**Purpose**: Advanced customer clustering and analysis

**Key Features**:
- **Consumer Clusters**: 4 distinct customer segments
- **Business Clusters**: 3 business customer segments
- **Interactive Clustering**: Adjustable cluster numbers
- **Download Capabilities**: Export plots and data

**How to Use**:
- Switch between Consumer and Business tabs
- Adjust cluster numbers using the slider (2-8)
- View elbow plots and silhouette scores for optimal clustering
- Download cluster visualizations and data tables

## Key Features Guide

### Interactive Filters
**Location**: Various tabs throughout the dashboard
**Purpose**: Narrow down data for focused analysis

**Common Filters**:
- **Date Range**: Select specific time periods
- **Year/Month**: Quick time-based filtering
- **Customer Segment**: Business vs Consumer
- **Country/Region**: Geographic filtering
- **Department**: Product category filtering

### Data Tables
**Features**:
- **Sorting**: Click column headers to sort
- **Searching**: Use the search box for quick filtering
- **Pagination**: Navigate through large datasets
- **Export**: Download data in various formats

### Interactive Maps
**Features**:
- **Zoom**: Use mouse wheel or zoom controls
- **Pan**: Click and drag to move around
- **Heatmap Intensity**: Color intensity shows sales volume
- **Legend**: Reference for color scale

### Clustering Analysis
**Consumer Segments**:
1. **High-Value New**: Recent customers with high spending
2. **Lapsed Frequent**: Previously active, now inactive customers
3. **Mid-Value New**: Moderate spending, recent customers
4. **Dormant Mid-Value**: Inactive customers with moderate value

**Business Segments**:
1. **Frequent Low-Value**: Regular customers with low spending
2. **High-Value New**: New business customers with high value
3. **Dormant Mid-Value**: Inactive business customers

## Analytics Insights

### Understanding KPIs
- **Total Sales**: Gross revenue from all transactions
- **Total Profit**: Net profit after costs
- **Total Orders**: Number of unique orders
- **Customers**: Number of unique customers

### Interpreting Clusters
- **Recency**: How recently customers made purchases
- **Frequency**: How often customers order
- **Monetary**: How much customers spend
- **Behavioral**: Order patterns and preferences

### Geographic Analysis
- **Sales Intensity**: Darker colors indicate higher sales
- **Customer Distribution**: Geographic spread of customers
- **Market Penetration**: Areas with high vs low activity

## Advanced Features

### Downloading Data
**Available Downloads**:
- Cluster plots as PNG files
- Cluster data as CSV files
- Filtered order data
- Product performance data

**How to Download**:
- Click download buttons in the Segmentation tab
- Use browser's "Save As" for tables
- Export filtered data for external analysis

### Custom Analysis
**Creating Custom Views**:
1. Use filters to narrow down data
2. Note the filtered results
3. Export data for external analysis
4. Combine multiple filter criteria

### Performance Optimization
**For Large Datasets**:
- Use specific date ranges
- Filter by customer segments
- Focus on specific geographic regions
- Use the clustering analysis for insights

## Troubleshooting

### Common Issues

**Dashboard Won't Start**:
- Check if all packages are installed
- Verify data files are in the correct location
- Ensure R version is 4.0 or higher

**Slow Performance**:
- Use filters to reduce data size
- Close other applications
- Consider using smaller date ranges

**Missing Data**:
- Check if data files are properly loaded
- Verify file paths and names
- Ensure data format matches expectations

**Clustering Errors**:
- Try different cluster numbers
- Check for data quality issues
- Verify all required features are present

### Getting Help
1. **Check the README**: For installation and setup issues
2. **Review Data Structure**: Ensure data files are correct
3. **Contact Support**: Create an issue in the repository
4. **Check Logs**: Look for error messages in R console

## Best Practices

### Data Analysis
- **Start Broad**: Begin with Executive Summary for overview
- **Drill Down**: Use filters to focus on specific areas
- **Compare Segments**: Use clustering for customer insights
- **Track Trends**: Monitor changes over time

### Dashboard Usage
- **Regular Monitoring**: Check KPIs frequently
- **Segment Analysis**: Use clustering for customer strategy
- **Geographic Insights**: Identify market opportunities
- **Product Performance**: Optimize product mix

### Reporting
- **Export Key Insights**: Download important visualizations
- **Document Findings**: Keep notes of significant discoveries
- **Share Results**: Use dashboard for presentations
- **Track Changes**: Monitor improvements over time

---

**Need Help?** Check the README.md file or create an issue in the repository. 