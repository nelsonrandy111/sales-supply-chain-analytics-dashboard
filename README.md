# Sales & Supply Chain Analytics Dashboard

A comprehensive R Shiny application for analyzing customer behavior, sales performance, and supply chain operations using advanced clustering algorithms and interactive visualizations.

## Features

### Executive Summary
- Real-time KPIs (Sales, Profit, Orders, Customers)
- Year-over-year performance tracking
- Top 5 departments and states by sales
- Monthly sales trend analysis

### Product Analysis
- Top 10 products and categories by sales
- Interactive product tables with filtering
- Revenue analysis by product category
- Year and month-based filtering

### Orders & Geographic Analysis
- Interactive heatmap showing sales distribution
- Order filtering by date range and order ID
- Geographic visualization of customer locations
- Detailed order tracking and analysis

### Sales Overview
- Profit vs. Average Discount correlation analysis
- Customer segment analysis (Business vs Consumer)
- Monthly KPIs with detailed metrics
- Advanced filtering by country, segment, and department

### Customer Segmentation
- **Consumer Clusters**: 4 distinct customer segments using K-means clustering
- **Business Clusters**: 3 business segments with outlier detection
- Interactive cluster analysis with adjustable parameters
- PCA visualization with confidence ellipses
- Download capabilities for cluster plots and data

## Installation

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Required R Packages
```r
# Core packages
install.packages(c("shiny", "bslib", "shinydashboard", "dplyr", "ggplot2", 
                   "lubridate", "DT", "leaflet", "tidyr", "cluster", 
                   "factoextra", "isotree", "scales", "tidyverse"))

# Additional packages for clustering
install.packages(c("fastDummies", "data.table", "lattice", "tidymodels"))
```

### Setup
1. Clone or download this repository
2. Ensure all data files are in the `data/` directory:
   - `data/DataCoSupplyChainDataset.csv` (main dataset)
   - `data/clustering_data.csv` (processed clustering data)
3. Open `app.R` in RStudio
4. Run the application with `shiny::runApp()`

## Project Structure

```
final_app/
├── app.R                          # Main Shiny application
├── clustering_cleaning.R          # Data preprocessing pipeline
├── customer_clusters.R            # Consumer customer clustering
├── business_cluster.R             # Business customer clustering
├── data/                          # Data directory
│   ├── DataCoSupplyChainDataset.csv  # Raw supply chain data
│   ├── clustering_data.csv            # Processed clustering data
│   └── README.md                      # Data documentation
├── docs/                          # Documentation
│   └── user_guide.md                  # User guide
├── .github/                       # GitHub templates
│   └── ISSUE_TEMPLATE/                # Issue templates
├── README.md                      # This file
├── .gitignore                     # Git ignore file
├── LICENSE                        # MIT License
├── CONTRIBUTING.md                # Contributing guidelines
├── CHANGELOG.md                   # Version history
├── install_packages.R             # Package installation script
└── final_app.Rproj               # RStudio project file
```

## Usage

### Running the Application
```r
# Method 1: From RStudio
# Open app.R and click "Run App"

# Method 2: From R console
shiny::runApp()

# Method 3: Specify port
shiny::runApp(port = 3838)
```

### Key Features Guide

#### Executive Summary
- Use the year filter to analyze specific time periods
- View top-performing departments and states
- Monitor sales trends and month-over-month comparisons

#### Customer Segmentation
- Adjust cluster numbers (2-8) using the slider
- View elbow plots and silhouette scores for optimal clustering
- Download cluster visualizations and data tables
- Analyze customer characteristics by cluster

#### Geographic Analysis
- Use the interactive map to identify high-sales regions
- Filter orders by date range and specific order IDs
- Analyze shipping patterns and delivery performance

## Analytics Methodology

### Customer Clustering
- **Consumer Segments**: 4 clusters using K-means with PCA
- **Business Segments**: 3 clusters with Isolation Forest outlier detection
- **Features**: RFM metrics, order behavior, discount patterns
- **Validation**: Elbow method and silhouette analysis

### Data Processing
- RFM (Recency, Frequency, Monetary) analysis
- Feature engineering for customer behavior patterns
- Anomaly detection for data quality
- Dimensionality reduction using PCA

## Customization

### Adding New Data Sources
1. Update `clustering_cleaning.R` to process new data
2. Modify feature engineering as needed
3. Update clustering parameters in respective files
4. Test with the main application

### Modifying Visualizations
- Edit plot functions in `app.R`
- Customize color schemes and themes
- Add new interactive elements

## Data Sources

- **Main Dataset**: `data/DataCoSupplyChainDataset.csv` (91MB)
  - Contains order details, customer information, product data
  - Includes geographic coordinates for mapping
  - Covers multiple years of transaction data
  - **Source**: [DataCo Smart Supply Chain for Big Data Analysis](https://www.kaggle.com/datasets/shashwatwork/dataco-smart-supply-chain-for-big-data-analysis) on Kaggle
  - **Original Author**: Shashwat Work
  - **License**: CC0: Public Domain

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

For questions or issues:
- Create an issue in the GitHub repository
- Contact the development team
- Check the documentation for common solutions

## Future Enhancements

- [ ] Machine learning predictions for customer churn
- [ ] Real-time data integration
- [ ] Advanced forecasting models
- [ ] Mobile-responsive design
- [ ] API integration for external data sources
- [ ] Automated reporting and alerts

---

**Built with R Shiny and Tidyverse** 