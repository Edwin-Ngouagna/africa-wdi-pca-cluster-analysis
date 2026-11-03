# ========================================================================
# MASTERCLASS: Pincipal Component Analysis (PCA) AND CLUSTER ANALYSIS IN R
# Dataset: Africa World Development Indicators (WDI)
# =======================================================================


# ============================================================
#                       PART I 
#             EXPLORING AND PREPARING THE DATA
# ============================================================

# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

install.packages("here") #The {here} package helps us locate files within our
# project folder without using an absolute file path.
library(here)

install.packages("tidyverse") #The {tidyverse} provides functions for data manipulation
# and exploration.
library(tidyverse)


# ------------------------------------------------------------
# 2. Import the dataset
# ------------------------------------------------------------

# Use here() to construct the path to the CSV file.
# The file should be saved inside the project folder.
africa_wdi <- read.csv(here("Data", "Africa_WDI_Data_CSV.csv"), header = TRUE)




# ------------------------------------------------------------
# 3. Get a first look at the data
# ------------------------------------------------------------

# Display the first six observations.
head(africa_wdi)

# Display the last six observations.
tail(africa_wdi)

#View the entire dataset
view(africa_wdi)

# Show the number of rows and columns.
dim(africa_wdi)
# ------------------------------------------------------
# 4. Missing Value analysis
# -------------------------------------------------------
missing_summary <- data.frame(
  Variable = names(africa_wdi),
  Missing_n = colSums(is.na(africa_wdi)),
  Missing_percent = colMeans(is.na(africa_wdi)) * 100
)

missing_summary

# Show values that may represent missing data, excluding genuine NA values
# NAs are not the only type of missing values so we must dig deeper
lapply(africa_wdi, function(x) 
  unique(x[!is.na(x) & x %in% c("..", ".", "", " ")]))

#Results show that the variable antiretroviral has another missing value ".."
# Count how many observations are coded as ".."
sum(africa_wdi$antiretroviral_HIV == "..")


# Identify the countries with ".." in antiretroviral_HIV
africa_wdi$country[africa_wdi$antiretroviral_HIV == ".."]


# Replace the non-standard missing value ".." with NA
africa_wdi$antiretroviral_HIV[
  africa_wdi$antiretroviral_HIV == ".."
] <- NA

# re-run to confirm they have been replace
lapply(africa_wdi, function(x) 
  unique(x[!is.na(x) & x %in% c("..", ".", "", " ")]))

#re-run missing_summary now and see the % of NAs
missing_summary <- data.frame(
  Variable = names(africa_wdi),
  Missing_n = colSums(is.na(africa_wdi)),
  Missing_percent = colMeans(is.na(africa_wdi)) * 100
)
missing_summary

#Turn into Latex Code for report writing in Overleaf
install.packages("stargazer")
library(stargazer)
stargazer(missing_summary, summary = FALSE, rownames = FALSE)


# Show variables with more than 20% missing data, so we note them and not include
# them in our model because their limited data coverage could reduce the reliability 
# and comparability of the resulting multivariate patterns.  
# This decision does not imply that these indicators are unimportant; 
# rather, it reflects the data-quality requirements of this specific analysis
missing_summary[missing_summary$Missing_percent > 20, ]

# ============================================================
# END OF PART 1 — INITIAL DATA EXPLORATION
# ============================================================


# =============================================================
#                       PART II - PCA 
# =============================================================

# --------------------------------------------------------------
# 1. Look at the Variables to be used more closely
# --------------------------------------------------------------

# Check the data types of the variables we are retaining
sapply(africa_wdi[, missing_summary$Missing_percent <= 20], class)

# Convert antiretroviral_HIV to numeric and select the numeric indicators
africa_wdi$antiretroviral_HIV <- as.numeric(africa_wdi$antiretroviral_HIV)


# Keep country names for identifying the results
country <- africa_wdi$country
country

# Exclude country_code from the analysis, so we select only numeric variabes, since country code is
# character, it stays off-shore
pca_data <- africa_wdi[sapply(africa_wdi, is.numeric)]

# Check the variables going into PCA
names(pca_data)


# Complete-case analysis would retain only countries with
# complete information across all 31 PCA variables.
# We check this first to understand how much of our sample
# would be lost if we simply excluded incomplete countries.
sum(complete.cases(pca_data))


# We see that only one country has complete data so we cannot use complet-case analysis.


# Count the number of missing values for each country.
# This shows how much information is missing within each
# country's 31 PCA variables.
country_missing <- rowSums(is.na(pca_data))
country_missing


# Summarize the distribution of missing values across
# countries using the minimum, quartiles, mean, and maximum.
# This helps us assess whether missingness is concentrated
# in a few countries or is generally limited.
summary(country_missing)

# Create a country-level missing-value table showing the
# number and percentage of missing values across the 31
# PCA variables for each country.
country_missing_summary <- data.frame(
  Country = country,
  Missing_n = country_missing,
  Missing_percent = (country_missing / ncol(pca_data)) * 100)
country_missing_summary

#Table worth putting in our report because it dicides what we do next
stargazer(country_missing_summary)

#The above gives just the mean, median, etc, let us make it tabular

stargazer(
  country_missing_summary,
  summary = FALSE,
  rownames = FALSE,
  title = "Missing Values by Country",
  label = "tab:country_missing"
)


# For each indicator, missing values are replaced with the
# median of the observed values for that indicator.
# The median is less sensitive to extreme values than the mean.

pca_data_imputed <- as.data.frame(
  lapply(
    pca_data,
    function(x) ifelse(
      is.na(x),
      median(x, na.rm = TRUE),
      x
    )
  )
)

# Verify that no missing values remain after imputation.
sum(is.na(pca_data_imputed))

# Examine the minimum, maximum, mean, and standard deviation
# of all 31 variables before standardization.

scale_check <- data.frame(
  Variable = names(pca_data_imputed),
  Minimum = sapply(pca_data_imputed, min, na.rm = TRUE),
  Maximum = sapply(pca_data_imputed, max, na.rm = TRUE),
  Mean = sapply(pca_data_imputed, mean, na.rm = TRUE),
  SD = sapply(pca_data_imputed, sd, na.rm = TRUE)
)
  
scale_check

#Grab a Latex code and add to the report
stargazer(
  scale_check,
  summary = FALSE,
  rownames = FALSE,
  title = "Scale assessment",
  label = "tab:scale_assessment"
)

# ---------------------------------------------------------
# 2. Standardizing the PCA Variables
# ---------------------------------------------------------

# Standardize each variable so that it has a mean of 0
# and a standard deviation of 1.
# This places all 31 indicators on a common scale
# before PCA is conducted.

pca_data_scaled <- as.data.frame(scale(pca_data_imputed))

# Check the standardized data.
pca_data_scaled

# Verify that each variable now has a mean close to 0
# and a standard deviation close to 1.

scale_verification <- data.frame(
  Variable = names(pca_data_scaled),
  Mean = sapply(pca_data_scaled, mean),
  SD = sapply(pca_data_scaled, sd)
)

scale_verification

stargazer(
  scale_verification,
  summary = FALSE,
  rownames = FALSE,
  title = "New Scale",
  label = "tab:new-scale"
)


# ---------------------------------------------------------
# 3.Assessment of Population-Structure Variables
# ---------------------------------------------------------

# Calculate the Pearson correlation between each pair of the
# 31 standardized development indicators. This allows us to
# examine whether the indicators share patterns of variation
# before conducting Principal Component Analysis (PCA).

cor_matrix <- cor(pca_data_scaled)

# Display the correlation matrix so that the strength and
# direction of relationships between the indicators can be inspected.
cor_matrix

install.packages("psych")
library(psych)

# Assess whether the 30 indicators share enough common
# variance to justify dimension reduction using PCA.
variance_check <- KMO(pca_data_scaled)
variance_check

# Extract the overall KMO measure from the KMO results.
kmo_table <- data.frame(
  Measure = "Overall KMO",
  Value = variance_check$MSA
)

# Export the KMO result as a LaTeX-ready table.
stargazer(
  kmo_table,
  summary = FALSE,
  rownames = FALSE,
  title = "Kaiser-Meyer-Olkin (KMO) Measure",
  label = "tab:kmo"
)


# Test whether the correlation matrix contains statistically
# significant relationships among the development indicators.
# A significant result suggests that the variables are not
# completely unrelated and therefore have shared structure.

# Run Bartlett's test of sphericity to assess whether
# the correlation matrix contains statistically significant
# relationships among the development indicators.
bartlett_result <- cortest.bartlett(
  cor_matrix,
  n = nrow(pca_data_scaled)
)

bartlett_result

# Extract the main Bartlett test results into a data frame
# so that they can be exported as a LaTeX table.
bartlett_table <- data.frame(
  Test = "Bartlett's test of sphericity",
  Chi_square = bartlett_result$chisq,
  df = bartlett_result$df,
  p_value = bartlett_result$p.value
)

# Export the Bartlett test results as a LaTeX-ready table.
stargazer(
  bartlett_table,
  summary = FALSE,
  rownames = FALSE,
  title = "Bartlett's Test of Sphericity",
  label = "tab:bartlett"
)


# ---------------------------------------------------------
# 4. Principal Component Analysis (PCA) Proper
# ---------------------------------------------------------

# Run PCA on the 30 standardized development indicators.
# Standardization was performed earlier so that variables
# measured on different scales contribute comparably.
pca_result <- prcomp(
  pca_data_scaled,
  center = FALSE,
  scale. = FALSE
)

# Display the PCA results.
summary(pca_result)


# Calculate the eigenvalue of each principal component.
eigenvalues <- pca_result$sdev^2

# Keep only components with eigenvalues greater than 1.
retained_components <- which(eigenvalues > 1)

# Display the retained component numbers.
retained_components

# Create a summary of the components retained by the
# eigenvalue-greater-than-one criterion.
pca_retained <- data.frame(
  Component = paste0("PC", retained_components),
  Eigenvalue = eigenvalues[retained_components],
  Variance = (eigenvalues[retained_components] /
                sum(eigenvalues)) * 100,
  Cumulative = cumsum(
    eigenvalues[retained_components] / sum(eigenvalues)
  ) * 100
)

# Round the results for easier interpretation.
pca_retained$Eigenvalue <- round(pca_retained$Eigenvalue, 3)
pca_retained$Variance <- round(pca_retained$Variance, 2)
pca_retained$Cumulative <- round(pca_retained$Cumulative, 2)

pca_retained

#put it in latex
stargazer(pca_retained,
          summary = FALSE,
          rownames = FALSE,
          title = "PCA results",
          label = "tab:new-scale")

# Plot the eigenvalues of the principal components.
# The scree plot helps identify where the major decline in
# explained variance occurs and therefore helps determine
# how many components should be retained.
plot(
  pca_result,
  type = "l",
  main = "Scree Plot of Principal Components"
)

# Examine how strongly each original indicator contributes
# to each principal component.
loadings <- pca_result$rotation[, 1:4]

loadings

# Add the indicator names to the PCA loading table
loading_table <- data.frame(
  Indicator = rownames(loadings),
  loadings
)

# Export the loading table with indicator names
stargazer(
  loading_table,
  summary = FALSE,
  rownames = FALSE,
  title = "PCA Results by indicator",
  label = "tab:pca_loadings"
)


# =============================================================
#                       PART III - CLUSTER ANALYSIS 
# =============================================================

# Extract the four retained component scores
pca_scores <- as.data.frame(pca_result$x[, 1:4])

# Add country names
pca_scores$country <- country

# Arrange country first, followed by the four components
pca_scores_table <- data.frame(
  country = pca_scores$country,
  PC1 = pca_scores$PC1,
  PC2 = pca_scores$PC2,
  PC3 = pca_scores$PC3,
  PC4 = pca_scores$PC4
)

# Check the table
pca_scores_table

stargazer(
  pca_scores_table,
  summary = FALSE,
  rownames = FALSE,
  title = "Country Scores on the Four Retained Principal Components",
  label = "tab:pca_scores"
)

# ---------------------------------------------------------
# Identify Countries in Each Cluster
# ---------------------------------------------------------

# Select the four retained principal component scores
# for clustering. Country is kept separately as the identifier.
cluster_data <- pca_scores_table[, c("PC1", "PC2", "PC3", "PC4")]

cluster_data

# Calculate distances between countries based on their
# positions across the four retained PCA components.
country_distance <- dist(cluster_data)
country_distance

hierarchical_model <- hclust(
  country_distance,
  method = "ward.D2"
)

# Plot the hierarchical clustering tree without country labels.
plot(
  hierarchical_model,
  labels = FALSE,
  main = "Hierarchical Clustering of African Countries",
  xlab = "Countries",
  sub = ""
)

library(cluster)

silhouette_results <- data.frame(
  Clusters = 2:6,
  Silhouette = sapply(
    2:6,
    function(k) {
      cluster_assignment <- cutree(
        hierarchical_model,
        k = k
      )
      
      mean(
        silhouette(
          cluster_assignment,
          country_distance
        )[, 3]
      )
    }
  )
)

silhouette_results


# ---------------------------------------------------------
#K-means Clustering
# ---------------------------------------------------------

# Six clusters were selected based on the highest
# average silhouette width.
k <- 6

# Set seed so that the results can be reproduced.
set.seed(123)

# Perform k-means clustering using the four retained
# principal component scores.
kmeans_model <- kmeans(
  cluster_data,
  centers = k,
  nstart = 25
)

# Create a table linking each country to its cluster.
cluster_results <- data.frame(
  country = pca_scores_table$country,
  Cluster = kmeans_model$cluster
)

# Arrange countries by cluster.
cluster_results <- cluster_results[
  order(cluster_results$Cluster),
]

# Display the result.
cluster_results

# ---------------------------------------------------------
# Identify Cameroon's Cluster
# ---------------------------------------------------------

cameroon_cluster <- cluster_results$Cluster[
  cluster_results$country == "Cameroon"
]

cameroon_cluster

# Show all countries belonging to Cameroon's cluster
cluster_results[
  cluster_results$Cluster == cameroon_cluster,
]  

#vCalculate the average of each original indicator
# among the 20 countries in Cameroon's cluster.

cameroon_cluster_profile <- sapply(
  pca_data_imputed[cluster_results$Cluster == cameroon_cluster, ],
  mean,
  na.rm = TRUE
)

cameroon_cluster_profile
  Cluster_Mean = sapply(
    cameroon_cluster_data,
    mean,
    na.rm = TRUE
  )
)

cameroon_cluster_profile


# Extract Cameroon's original indicator values
cameroon_values <- pca_data_imputed[country == "Cameroon", ]

# Compare Cameroon with the Cluster 5 average
cameroon_comparison <- data.frame(
  Variable = names(pca_data_imputed),
  Cameroon = as.numeric(cameroon_values),
  Cluster_Mean = as.numeric(cameroon_cluster_profile)
)

cameroon_comparison$Difference <-
  cameroon_comparison$Cameroon -
  cameroon_comparison$Cluster_Mean

cameroon_comparison
