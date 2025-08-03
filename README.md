# SDG Index Prediction Project

A comprehensive data science project focused on analyzing and predicting Sustainable Development Goals (SDG) Index scores using machine learning techniques.

## 🎯 Project Overview

This project implements a complete data science pipeline for analyzing global SDG performance data and building predictive models to forecast SDG Index scores. The project demonstrates proficiency in data preprocessing, exploratory data analysis, and machine learning model development using Python.

## 📁 Project Structure

```
Group8_LamTuanThinh_Python/
├── 01_Preprocess.ipynb    # Data cleaning and preprocessing pipeline
├── 02_EDA.ipynb          # Exploratory Data Analysis and visualization
├── 03_Model.ipynb        # Machine learning model development
└── README.md             # Project documentation
```

## 🔧 Technologies Used

- **Python**: Primary programming language
- **Pandas**: Data manipulation and analysis
- **NumPy**: Numerical computing
- **Matplotlib/Seaborn**: Data visualization
- **Scikit-learn**: Machine learning algorithms and preprocessing

## 📊 Dataset

The project utilizes the Sustainable Development Report 2024 dataset, which includes:
- SDG Index scores for multiple countries and years
- Various indicators across all 17 SDGs
- Time series data enabling temporal analysis
- Comprehensive country-level development metrics

## 🚀 Project Workflow

### 1. Data Preprocessing (`01_Preprocess.ipynb`)
- **Data Loading**: Import raw SDG data from Excel files
- **Data Merging**: Combine multiple data sources (Raw Data, Backdated SDG Index, Full Database)
- **Missing Value Analysis**: Identify and handle missing data patterns
- **Data Cleaning**: Remove irrelevant columns and handle data quality issues
- **Outlier Detection**: Analyze and process extreme values
- **Feature Engineering**: Create derived variables for enhanced model performance

### 2. Exploratory Data Analysis (`02_EDA.ipynb`)
- **Descriptive Statistics**: Comprehensive statistical summary of all variables
- **SDG Grouping**: Organize indicators by SDG categories (SDG1-SDG17)
- **Distribution Analysis**: Examine data distributions across variables
- **Correlation Analysis**: Identify relationships between different indicators
- **Temporal Patterns**: Analyze trends over time
- **Geographical Insights**: Explore regional variations in SDG performance

### 3. Machine Learning Models (`03_Model.ipynb`)
- **Data Scaling**: MinMax normalization (0-100 scale) for consistent feature ranges
- **Feature Engineering**: Create lag variables to capture temporal dependencies
- **Model Development**: Implement predictive models for SDG Index forecasting
- **Model Evaluation**: Assess performance using appropriate metrics
- **Visualization**: Generate plots to illustrate model results and insights

## 🎯 Key Features

- **Comprehensive Data Pipeline**: End-to-end workflow from raw data to predictions
- **Time Series Handling**: Innovative approach using lag variables for ML models
- **Multi-dimensional Analysis**: Examination across countries, time, and SDG categories
- **Professional Documentation**: Well-structured and commented code
- **Reproducible Results**: Clear workflow enabling result replication

## 📈 Project Outcomes

- **Data Insights**: Identified key patterns in global SDG performance
- **Predictive Model**: Developed ML model capable of forecasting SDG Index scores
- **Technical Skills**: Demonstrated proficiency in Python data science stack
- **Problem-Solving**: Applied creative solutions for time series data in ML context

## 🎓 Academic Context

- **Course**: Business Analytics (BA) - Semester 6
- **Institution**: University Project
- **Team**: Group 8 - Lam Tuan Thinh
- **Focus**: Practical application of data science in sustainable development analysis

## 💡 Low-Code Application Relevance

This project demonstrates several skills valuable for low-code platform development:

- **Data Integration**: Experience with multiple data sources and formats
- **Workflow Automation**: Structured, repeatable data processing pipelines
- **Visual Analytics**: Creation of meaningful visualizations and dashboards
- **User-Centric Design**: Focus on interpretable results and clear documentation
- **Scalable Solutions**: Modular approach suitable for platform integration

## 🔄 How to Run

1. **Setup Environment**:
   ```python
   pip install pandas numpy matplotlib seaborn scikit-learn openpyxl
   ```

2. **Execute Notebooks**:
   - Run `01_Preprocess.ipynb` for data preparation
   - Run `02_EDA.ipynb` for analysis and insights
   - Run `03_Model.ipynb` for model development

3. **Data Requirements**:
   - Ensure `SDR2024-data.xlsx` is in the project directory
   - Verify all required sheets are present in the Excel file

## 👨‍💻 Author

**Lam Tuan Thinh**
- Business Analytics Student
- Passionate about data science and sustainable development
- Seeking opportunities in low-code platform development

## 🌟 Project Highlights

- **Complete ML Pipeline**: From data ingestion to model deployment
- **Real-world Impact**: Focus on sustainable development goals
- **Technical Excellence**: Clean, well-documented, and professional code
- **Innovation**: Creative approach to time series analysis in ML context
- **Scalability**: Modular design suitable for platform integration

---

*This project showcases my ability to work with complex datasets, implement machine learning solutions, and create professional-grade data science projects suitable for low-code platform environments.*
