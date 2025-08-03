# SDG Data Processing

# Input and Data Loading
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
# Read data

data <- list()
data[["Raw Data - Panel"]] <- read_excel('data/raw/SDR2024-data.xlsx', sheet = "Raw Data - Panel")
data[["Backdated SDG Index"]] <- read_excel('data/raw/SDR2024-data.xlsx', sheet = "Backdated SDG Index")
data[["Full Database"]] <- read_excel('data/raw/SDR2024-data.xlsx', sheet = "Full Database")
data[["Codebook"]] <- read_excel('data/raw/SDR2024-data.xlsx', sheet = "Codebook")
# Examine raw data
raw_data <- data[["Raw Data - Panel"]]
head(raw_data, 10)
colnames(raw_data)
str(raw_data)

# Merging Data
backdated_data <- data[["Backdated SDG Index"]]
fulldata <- data[["Full Database"]]
codebook <- data[["Codebook"]]
# Extract required columns from backdated data
backdated_data <- backdated_data %>% 
  select(id, Country, year, `SDG Index Score`)
# Merge datasets
dataset <- raw_data %>% 
  left_join(backdated_data, by = c("Country", "year")) %>%
  select(-id.x, -id.y, -indexreg)
print(head(dataset))

# Lặp qua từng dòng của codebook để ánh xạ giữa Indicator và IndCode
for (i in 1:nrow(codebook)) {
  indicator_name <- codebook$Indicator[i]
  ind_code <- codebook$IndCode[i]
  
  if (indicator_name %in% colnames(fulldata) && ind_code %in% colnames(dataset)) {
    year_col <- paste0("Year: ", ind_code)
    
    # Kiểm tra nếu cột year_col tồn tại
    if (year_col %in% colnames(fulldata)) {
      temp_df <- fulldata %>%
        select(all_of(indicator_name), all_of(year_col), Country) %>%
        filter(.data[[year_col]] == 2024)
      
      # Với mỗi Country phù hợp, gán giá trị vào dataset
      for (j in 1:nrow(temp_df)) {
        country <- temp_df$Country[j]
        value <- temp_df[[indicator_name]][j]
        
        # Xác định dòng trong dataset tương ứng với Country và year = 2024
        mask <- which(!is.na(dataset$Country) & !is.na(dataset$year) &
                      dataset$Country == country & dataset$year == 2024)
        
        if (length(mask) > 0) {
          dataset[mask, ind_code] <- value
        }
      }
    }
  }
}

# Gán giá trị '2024 SDG Index Score' nếu có
if ("2024 SDG Index Score" %in% colnames(fulldata) && "SDG Index Score" %in% colnames(dataset)) {
  for (i in 1:nrow(fulldata)) {
    country <- fulldata$Country[i]
    score <- fulldata[["2024 SDG Index Score"]][i]
    
    mask <- which(!is.na(dataset$Country) & !is.na(dataset$year) &
                  dataset$Country == country & dataset$year == 2024)
    
    if (length(mask) > 0) {
      dataset[mask, "SDG Index Score"] <- score
    }
  }
}


# Filling missing data based on dataset's codebook

# sdg2_undernsh: 2.5% for high-income countries
sum(is.na(dataset$sdg2_undernsh))

# Read high-income countries data
high_income_country <- read.csv('data/raw/world-bank-income-groups.csv')
print(head(high_income_country))

# Filter for high-income countries
high_income <- high_income_country %>%
  filter(`World.Bank.s.income.classification` == "High-income countries")

# Loop through each row of high_income and assign value
for (i in 1:nrow(high_income)) {
  country <- high_income$Entity[i]
  year <- high_income$Year[i]
  
  mask <- dataset$Country == country & dataset$year == year
  dataset$sdg2_undernsh[mask] <- 2.5
}

sum(is.na(dataset$sdg2_undernsh))

# sdg2_stunting
sum(is.na(dataset$sdg2_stunting))

# Loop through each row of high_income and assign value
for (i in 1:nrow(high_income)) {
  country <- high_income$Entity[i]
  year <- high_income$Year[i]
  
  mask <- dataset$Country == country & dataset$year == year
  dataset$sdg2_stunting[mask] <- 2.58
}

sum(is.na(dataset$sdg2_stunting))

# sdg2_wasting
sum(is.na(dataset$sdg2_wasting))

# Loop through each row of high_income and assign value
for (i in 1:nrow(high_income)) {
  country <- high_income$Entity[i]
  year <- high_income$Year[i]
  
  mask <- dataset$Country == country & dataset$year == year
  dataset$sdg2_wasting[mask] <- 0.75
}

sum(is.na(dataset$sdg2_wasting))

# sdg5_familypl
sum(is.na(dataset$sdg5_familypl))

# Read family planning data
familypl <- read.csv('data/raw/Data_FamilyPlanningIndicators_2024.csv', fileEncoding = 'ISO-8859-1')

# Get unique countries
countries_dataset <- unique(dataset$Country)
locations_familypl <- unique(familypl$Location)

# Countries in dataset but not in familypl
only_in_dataset <- setdiff(countries_dataset, locations_familypl)

# Countries in familypl but not in dataset
only_in_familypl <- setdiff(locations_familypl, countries_dataset)

cat("In dataset but not in familypl:\n")
cat(sort(only_in_dataset), sep = "\n")

cat("\nIn familypl but not in dataset:\n")
cat(sort(only_in_familypl), sep = "\n")

# Country name mapping
country_name_mapping <- c(
  'Bahamas, The' = 'Bahamas',
  'Bolivia' = 'Bolivia (Plurinational State of)',
  'Brunei Darussalam' = 'Brunei Darussalam',
  'Congo, Dem. Rep.' = 'Dem. Rep. of the Congo',
  'Congo, Rep.' = 'Congo',
  "Cote d'Ivoire" = "Côte d'Ivoire",
  'Egypt, Arab Rep.' = 'Egypt',
  'Gambia, The' = 'Gambia',
  'Iran, Islamic Rep.' = 'Iran (Islamic Republic of)',
  'Korea, Dem. Rep.' = "Dem. People's Rep. of Korea",
  'Korea, Rep.' = 'Republic of Korea',
  'Kyrgyz Republic' = 'Kyrgyzstan',
  'Lao PDR' = "Lao People's Dem. Republic",
  'Micronesia, Fed. Sts.' = 'Micronesia',
  'Moldova' = 'Republic of Moldova',
  'Slovak Republic' = 'Slovakia',
  'St. Kitts and Nevis' = 'Saint Kitts and Nevis',
  'St. Lucia' = 'Saint Lucia',
  'St. Vincent and the Grenadines' = 'Saint Vincent and the Grenadines',
  'Tanzania' = 'United Republic of Tanzania',
  'United States' = 'United States of America',
  'Venezuela, RB' = 'Venezuela (Bolivarian Republic of)',
  'Vietnam' = 'Viet Nam',
  'Yemen, Rep.' = 'Yemen'
)

# Filter familypl for median only
familypl_median <- familypl %>% filter(Variant == 'Median')

# Replace country names using the mapping
for (i in 1:length(country_name_mapping)) {
  familypl_median$Location[familypl_median$Location == names(country_name_mapping)[i]] <- country_name_mapping[i]
}

# Loop through each row and assign value
for (i in 1:nrow(familypl_median)) {
  country <- familypl_median$Location[i]
  year <- familypl_median$Time[i]
  value <- familypl_median$Value[i]
  
  mask <- dataset$Country == country & dataset$year == year
  dataset$sdg5_familypl[mask] <- value
}

sum(is.na(dataset$sdg5_familypl))

# sdg6_safewat
sum(is.na(dataset$sdg6_safewat))

# Loop by year in dataset
years <- unique(dataset$year)

for (y in years) {
  # Get New Zealand value for year y
  value_nz <- dataset %>% 
    filter(Country == 'New Zealand', year == y) %>% 
    pull(sdg6_safewat)
  
  if (length(value_nz) > 0 && !is.na(value_nz[1])) {
    value <- value_nz[1]
    # Assign value to Australia for same year
    dataset$sdg6_safewat[dataset$Country == 'Australia' & dataset$year == y] <- value
  }
}

sum(is.na(dataset$sdg6_safewat))

# sdg9_uni
sum(is.na(dataset$sdg9_uni))

# Process University Ranking data
library(dplyr)

all_rankings <- data.frame()

for (year in 2011:2024) {
  file_path <- paste0("data/raw/World_University_Rankings/",year, "_rankings.csv")
  df <- read.csv(file_path)
  
  # Ensure relevant columns are numeric
  numeric_cols <- c(
    "scores_teaching",
    "scores_research",
    "scores_citations",
    "scores_industry_income",
    "scores_international_outlook"
  )
  
  for (col in numeric_cols) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }
  
  # Calculate overall score
  df$score <- df$scores_teaching * 0.3 +
              df$scores_research * 0.3 +
              df$scores_citations * 0.3 +
              df$scores_international_outlook * 0.075 +
              df$scores_industry_income * 0.025
  
  # Add year column
  df$year <- year
  
  # Rename location column to country
  names(df)[names(df) == "location"] <- "country"
  
  # Keep only necessary columns
  df <- df %>% select(name, country, score, year)
  
  all_rankings <- rbind(all_rankings, df)
}

full_ranking <- all_rankings

str(full_ranking)


# Function to calculate average of top 3 or less
avg_top_3_or_less <- function(scores) {
  scores_sorted <- sort(scores, decreasing = TRUE)
  return(mean(scores_sorted[1:min(3, length(scores_sorted))]))
}

# Calculate average by country and year
avg_scores_by_country_year <- full_ranking %>%
  group_by(country, year) %>%
  summarize(sdg9_uni = avg_top_3_or_less(score), .groups = "drop")

# Create mapping from (country, year) to sdg9_uni
mapping <- setNames(avg_scores_by_country_year$sdg9_uni, 
                   paste(avg_scores_by_country_year$country, avg_scores_by_country_year$year, sep = "_"))

# Assign values to dataset at missing sdg9_uni positions
mask_nan <- is.na(dataset$sdg9_uni)
for (i in which(mask_nan)) {
  country <- dataset$Country[i]
  year <- dataset$year[i]
  key <- paste(country, year, sep = "_")
  
  if (key %in% names(mapping)) {
    dataset$sdg9_uni[i] <- mapping[key]
  } else {
    dataset$sdg9_uni[i] <- 0
  }
}

sum(is.na(dataset$sdg9_uni))

# sdg9_rdex
sum(is.na(dataset$sdg9_rdex))

# Filter for low-income countries
low_income <- high_income_country %>%
  filter(`World.Bank.s.income.classification` == "Low-income countries")

# Loop through each row of low_income and assign value
for (i in 1:nrow(low_income)) {
  country <- low_income$Entity[i]
  year <- low_income$Year[i]
  
  mask <- dataset$Country == country & dataset$year == year
  dataset$sdg9_rdex[mask] <- 0
}

sum(is.na(dataset$sdg9_rdex))

# sdg11_slums
sum(is.na(dataset$sdg11_slums))

# Loop through high-income countries
for (i in 1:nrow(high_income)) {
  country <- high_income$Entity[i]
  year <- high_income$Year[i]
  
  mask <- dataset$Country == country & dataset$year == year
  dataset$sdg11_slums[mask] <- 0
}

sum(is.na(dataset$sdg11_slums))

# sdg16_clabor
sum(is.na(dataset$sdg16_clabor))

# Loop through high-income countries
for (i in 1:nrow(high_income)) {
  country <- high_income$Entity[i]
  year <- high_income$Year[i]
  
  mask <- dataset$Country == country & dataset$year == year
  dataset$sdg16_clabor[mask] <- 0
}

sum(is.na(dataset$sdg16_clabor))

# Check remaining null values
colSums(is.na(dataset))

# Drop columns/records if needed
# Drop rows with missing SDG Index Score
dataset <- dataset %>% drop_na(`SDG Index Score`)

# Analyze missing data
analyze_missing_data <- function(df) {
  missing_percent <- colMeans(is.na(df)) * 100
  cat("Missing value percentages per column:\n")
  print(sort(missing_percent, decreasing = TRUE))
  return(missing_percent)
}

missing_percent <- analyze_missing_data(dataset)

# Identify important columns with high missing values
identify_important_high_missing_cols <- function(df, target_column, 
                                                missing_threshold = 0.6, 
                                                correlation_threshold = 0.7) {
  # Calculate missing value percentages
  missing_percent <- colMeans(is.na(df))
  
  # Calculate correlations with target (only for numeric columns)
  numeric_cols <- sapply(df, is.numeric)
  correlations <- cor(df[, numeric_cols], use = "pairwise.complete.obs")[target_column, ]
  correlations <- abs(correlations)
  
  # Identify important columns with high missing values
  high_missing_cols <- names(missing_percent)[missing_percent > missing_threshold]
  important_cols <- names(correlations)[correlations > correlation_threshold]
  
  # Intersection of the two sets
  important_high_missing <- intersect(high_missing_cols, important_cols)
  
  return(important_high_missing)
}

important_cols <- identify_important_high_missing_cols(dataset, target_column = 'SDG Index Score')
cat("Important columns with >60% missing values:", important_cols, "\n")

# Drop non-important columns with high missing values
drop_non_important_high_missing <- function(df, important_cols, missing_threshold = 0.6) {
  # Calculate missing percentage for each column
  missing_percent <- colMeans(is.na(df))
  
  # Columns with missing > threshold
  high_missing_cols <- names(missing_percent)[missing_percent > missing_threshold]
  
  # Exclude important columns from the drop list
  cols_to_drop <- setdiff(high_missing_cols, important_cols)
  
  # Drop the columns
  df_dropped <- df %>% select(-all_of(cols_to_drop))
  
  cat("Drop columns:", cols_to_drop, "\n")
  # Return cleaned dataframe
  return(df_dropped)
}

dataset <- drop_non_important_high_missing(dataset, important_cols)
cat("New shape:", dim(dataset), "\n")

# Calculate NaN counts in each column
nan_counts <- sort(colSums(is.na(dataset)))
print(nan_counts)

# Remove 'Country' and 'year' before calculating correlation
dataset_without_country_year <- dataset %>% select(-Country, -year)

# Calculate correlation between all columns and 'SDG Index Score'
correlation_with_sdg <- cor(dataset_without_country_year, use = "pairwise.complete.obs")

# Filter correlation coefficients of each column with 'SDG Index Score'
correlation_with_sdg_index_score <- correlation_with_sdg[, "SDG Index Score"]
print(correlation_with_sdg_index_score)

# Plot heatmap to show correlation with 'SDG Index Score'
library(ggplot2)
library(reshape2)

# Convert correlation to data frame for plotting
cor_df <- data.frame(
  variable = names(correlation_with_sdg_index_score),
  correlation = correlation_with_sdg_index_score
) %>%
  arrange(desc(correlation))

# Plot heatmap
ggplot(cor_df, aes(x = 1, y = variable, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                      limits = c(-1, 1)) +
  geom_text(aes(label = sprintf("%.2f", correlation)), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y = "", fill = "Correlation", title = "Correlation with SDG Index Score")

# Filter columns with correlation >= 0.4
columns_to_keep <- names(correlation_with_sdg_index_score[abs(correlation_with_sdg_index_score) >= 0.4])

# Keep columns with correlation >= 0.4 and create new dataset
dataset_after_drop_cols <- dataset %>%
  select(Country, year, all_of(columns_to_keep))

# Print result dimensions
dim(dataset_after_drop_cols)

# Filling missing data with interpolation & Kmeans
# Linear interpolation

# Get numeric columns (except 'Country' and 'year')
numeric_cols <- names(dataset_after_drop_cols)[sapply(dataset_after_drop_cols, is.numeric)]
numeric_cols <- setdiff(numeric_cols, "year")

# Loop through each country in dataset
for (country in unique(dataset_after_drop_cols$Country)) {
  # Filter data for each country
  mask <- dataset_after_drop_cols$Country == country
  
  # Apply interpolation to numeric columns
  for (col in numeric_cols) {
    # Check if there are at least 2 non-NA values
    if (sum(!is.na(dataset_after_drop_cols[mask, col])) >= 2) {
      # Only interpolate if we have enough non-NA values
      dataset_after_drop_cols[mask, col] <- 
        approx(x = dataset_after_drop_cols$year[mask], 
               y = dataset_after_drop_cols[[col]][mask], 
               xout = dataset_after_drop_cols$year[mask],
               rule = 2)$y
    }
    # If not enough non-NA values, leave as is
  }
}

# Print result
print(dataset_after_drop_cols)

# Check remaining missing values after interpolation
missing_values <- colSums(is.na(dataset_after_drop_cols))
cat("Number of missing values in columns:\n")
print(missing_values)

# Clustering countries to fill missing values with KMeans
library(cluster)
library(factoextra)

# Get numeric columns, excluding 'year' and 'SDG Index Score'
numeric_cols <- names(dataset_after_drop_cols)[sapply(dataset_after_drop_cols, is.numeric)]
numeric_cols <- setdiff(numeric_cols, c("year", "SDG Index Score"))

# Clone data from dataset_after_drop_cols
df3 <- dataset_after_drop_cols

# Record original NaN mask to know where to fill
nan_mask <- sapply(df3[numeric_cols], is.na)

# Temporarily fill NaNs with -1
for (col in numeric_cols) {
  df3[[col]][is.na(df3[[col]])] <- -1
}

# Normalize data (scale only numeric_cols)
scale_data <- function(df, cols) {
  for (col in cols) {
    min_val <- min(df[[col]])
    max_val <- max(df[[col]])
    df[[col]] <- ((df[[col]] - min_val) / (max_val - min_val)) * 100
  }
  return(df)
}

df3 <- scale_data(df3, numeric_cols)

# Restore original values for 'year' and 'SDG Index Score'
df3$year <- dataset_after_drop_cols$year
df3$`SDG Index Score` <- dataset_after_drop_cols$`SDG Index Score`

# Clustering by year
df3$Cluster <- -1
for (year in unique(df3$year)) {
  year_mask <- df3$year == year
  data_year <- df3[year_mask, numeric_cols]
  
  # Change -1 temporarily to 0 to avoid KMeans errors
  for (col in numeric_cols) {
    data_year[[col]][data_year[[col]] == -1] <- 0
  }
  
  tryCatch({
    # Convert to matrix for kmeans
    data_matrix <- as.matrix(data_year)
    
    # Run kmeans
    km <- kmeans(data_matrix, centers = 4, nstart = 25)
    df3$Cluster[year_mask] <- km$cluster
  }, error = function(e) {
    cat("Cannot cluster for year", year, ":", e$message, "\n")
  })
}

# Fill missing values from clusters
for (col in numeric_cols) {
  for (cluster_id in unique(df3$Cluster)) {
    if (cluster_id == -1) next
    
    # Calculate mean of non-missing values in this cluster
    mask_values <- df3$Cluster == cluster_id & df3[[col]] != -1
    if (sum(mask_values) > 0) {
      cluster_mean <- mean(df3[[col]][mask_values])
      
      # Fill in original NaN positions
      fill_mask <- df3$Cluster == cluster_id & nan_mask[, col]
      df3[[col]][fill_mask] <- cluster_mean
    }
  }
}

# Check if any NaNs remain
cat("Any NaNs remaining after imputation:", sum(sapply(df3[numeric_cols], function(x) sum(is.na(x)))), "\n")

# Plot distribution before and after imputation
library(gridExtra)

plot_distributions <- function() {
  plots <- list()
  
  for (i in seq_along(numeric_cols)) {
    col <- numeric_cols[i]
    
    # Before imputation plot
    p1 <- ggplot(dataset_after_drop_cols, aes_string(x = col)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(title = paste("Before Imputation:", col),
           x = col, y = "Density") +
      theme_minimal()
    
    # After imputation plot
    p2 <- ggplot(df3, aes_string(x = col)) +
      geom_density(fill = "green", alpha = 0.5) +
      labs(title = paste("After Imputation:", col),
           x = col, y = "Density") +
      theme_minimal()
    
    plots[[2*i-1]] <- p1
    plots[[2*i]] <- p2
  }
  
  do.call(grid.arrange, c(plots, ncol = 2))
}

plot_distributions()

# Remove Cluster column and save dataset
df3 <- df3 %>% select(-Cluster)

# Save dataset to CSV

write.csv(df3, 'new_dataset.csv', row.names = FALSE)

message('Hoàn thành tiền xử lý dữ liệu!')

