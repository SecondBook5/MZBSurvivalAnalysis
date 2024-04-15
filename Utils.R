#advanced_summary.R
#module containing Advanced statistics for Numerical and categorical variables
library(psych)
library(gt)
library(tidyverse)

#' @title Calculate Advanced Statistics for Numeric Variables
#'
#' @description
#' This function calculates advanced statistics for numeric variables, including
#' descriptive statistics, quantiles, percentiles, and normality statistics.
#'
#' @param data A data frame.
#' @param selected_labels A character vector specifying the selected subsets of the table.
#'   Options include "Descriptive Statistics," "Quantiles," "Percentiles," and "Normality."
#' @param percentiles Numeric vector specifying the percentiles to include in the "Percentiles" table.
#'   Values should be in the range [0, 100].
#'
#' @return A gt table with advanced statistics for numeric variables, optionally subset based on selected labels.
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(tidyverse)
#' library(gt)
#'
#' # Load the data frame
#' df <- read_csv("path/to/your/data.csv")
#'
#' # Calculate advanced statistics for numeric variables
#' calc_num_stats(df)
#' # Return the "Descriptive Statistics" label and append "Quantiles"
#' calc_num_stats(df, selected_labels = c("Descriptive Statistics", "Quantiles"))
#' }
#'
#' @export
#' @importFrom gt gt tab_spanner fmt_number
#' @importFrom stats mean sd quantile skew kurtosis


calc_num_stats <- function(data, selected_labels = NULL, percentiles = NULL, title = "Advanced Statistics for Numeric Variables") {
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")                                  #error handling for wrong input
  }
  
  # Check if selected_labels is a character vector
  if (!is.null(selected_labels) && !is.character(selected_labels)) {
    stop("Input 'selected_labels' must be a character vector.")
  }
  
  # Check if percentiles is a numeric vector
  if (!is.null(percentiles) && !is.numeric(percentiles)) {
    stop("Input 'percentiles' must be a numeric vector.")
  }
  
 title <- paste(title)
  
  numeric_vars <- select_if(data, is.numeric)  %>%
    select_if(~ n_distinct(.) > 10)                                    #Determine the continuous numeric variables in a data frame
  
  # Convert percentiles to [0, 1] range if provided in [0, 100] range
  percentiles <- if (!is.null(percentiles)) percentiles / 100 else NULL
  
  
  # Check if selected_labels are valid
  valid_labels <- c("Descriptive Statistics", "Quantiles", "Percentiles", "Normality")
  if (!all(is.null(selected_labels) | selected_labels %in% valid_labels)) {
    stop("Invalid 'selected_labels'. Please choose from: ", paste0('"', valid_labels, '"', collapse = ", "))
  }
  
  #Calculate descriptive statistics
     
  descriptive_stats <- map_df(names(numeric_vars), function(var) {               #takes the names of numeric variables in our dataset using names, it then iterates over each element of the vector and applies a function to calculate its descriptive statistics, and returns a data frame using map_df
    values <- data[[var]]                                                        #extracts the values of the current numeric variable (var) from the dataset (data). It uses double brackets [[ for subsetting to get the column corresponding to the variable.
    n <- sum(!is.na(values))                                                     #sum of the number of non-missing values for the variable gives you the population size (n)
    
    # Create a tibble with descriptive statistics for the current variable
    tibble(
      Variable = var, 
      n = n,
      Mean = mean(values, na.rm = TRUE),                                         #Mean: average value of numeric variable
      Std_Dev = sd(values, na.rm = TRUE),                                        #Standard Deviation (SD): the amount of variation in a set of values, higher the SD greater the variability
      Std_Err = sd(values, na.rm = TRUE) / sqrt(n),                              #standard error of the mean (SEM): an estimate of how much a sample mean is expected to vary from the true population mean. (SD of a sample/ square root of the sample size(n))
      CI_Lower = mean(values, na.rm = TRUE) - stats::qt(0.975, df = n - 1) * (sd(values, na.rm = TRUE) / sqrt(n)), #confidence interval: provides a range within the true population parameter is likely to fall
      CI_Upper = mean(values, na.rm = TRUE) + stats::qt(0.975, df = n - 1) * (sd(values, na.rm = TRUE) / sqrt(n))  #calculated as the Mean +/- a critical value * the standard error. In this case we use the t-distribution with 95% level of confidence stats::qt(0.975, df = n - 1)
    )
  })
  
  # Default tibble for "Descriptive Statistics" 
  result <- descriptive_stats                                                   #When all values are left blank this tibble is the default
  
  # Append "Quantiles" tibble
  if ("Quantiles" %in% selected_labels) {
    quantiles_stats <- map_df(names(numeric_vars), function(var) {
      values <- data[[var]]
      tibble(
        Median = median(values, na.rm = TRUE),                                  #Median: measure of central tendency that is not affected by outlier.
        IQR = IQR(values, na.rm = TRUE),                                                   #The IQR is a measure of statistical dispersion, it represents the spread of the middle 50% of the data.
        IQR_Range = sprintf("%.1f - %.1f", quantile(values, 0.25, na.rm = TRUE), quantile(values, 0.75, na.rm = TRUE))
        #IQR_Range = paste(quantile(values, c(0.25, 0.75), na.rm = TRUE), collapse = " - ") #IQR is calculated as the difference between the third quartile (Q3, the value below which 75% of the data falls) and the first quartile (Q1, the value below which 25% of the data falls).
      )
    })
    
    result <- bind_cols(result, quantiles_stats)
  }
  
  # Append "Percentiles" tibble
  if ("Percentiles" %in% selected_labels) {
    percentiles_stats <- map_df(names(numeric_vars), function(var) {
      values <- data[[var]]
      
      if (!is.null(percentiles) && length(percentiles) > 0) {
        selected_percentiles <- quantile(values, percentiles, na.rm = TRUE)     #calculate a selected percentile when specified
        tibble(                                                                 #Create a tibble with named columns based on percentiles
          !!!setNames(selected_percentiles, paste0("p", percentiles * 100))
        )
      } else {
        #if percentile is unspecified the default is to return a tibble with some key percentiles
        tibble(
          p25 = quantile(values, 0.25, na.rm = TRUE),
          p50 = quantile(values, 0.50, na.rm = TRUE),
          p75 = quantile(values, 0.75, na.rm = TRUE),
          p33 = quantile(values, 0.33, na.rm = TRUE),
          p67 = quantile(values, 0.67, na.rm = TRUE),
          p01 = quantile(values, 0.01, na.rm = TRUE),
          p05 = quantile(values, 0.05, na.rm = TRUE),
          p10 = quantile(values, 0.10, na.rm = TRUE),
          p90 = quantile(values, 0.90, na.rm = TRUE),
          p95 = quantile(values, 0.95, na.rm = TRUE),
          p99 = quantile(values, 0.99, na.rm = TRUE)
        )
      }
    })
    
    result <- bind_cols(result, percentiles_stats)
  }
  
  # Function to calculate normality statistics
  calculate_normality_stats <- function(values) {
    skew <- skewness(values, na.rm = TRUE)                                      #Skewness: Measures the asymmetry of the distribution and which direction
    skew_type <- ifelse(skew < 0, "left-skewed", ifelse(skew > 0, "right-skewed", "symmetric"))   #Left Skewed is when skewness is negative, 
                                                                                                  #Right skewed is when skewness is positive 
                                                                                                  #symmetric is when skewness is close to 0 
    kurt <- kurtosis(values, na.rm = TRUE)                                      #Kurtosis measures the tailedness of a distribution, how much data falls in the lower percentiles
    kurt_type <- ifelse(kurt < 0, "platykurtic", ifelse(kurt > 0, "leptokurtic", "mesokurtic")) #Platykurtic is negative, lighter tails is flatter than normal distribution. 
                                                                                                #leptokurtic is positive, heavier tails more peaked than normal distribution. 
    # Create a tibble with normality statistics                                                 #mesokurtic is close to 0 the distribution is normal
    tibble(
      Skewness = skew,
      Skewness_Type = skew_type,
      Kurtosis = kurt,
      Kurtosis_Type = kurt_type
    )
  }
  
  # Append "Normality" tibble 
  if ("Normality" %in% selected_labels) {
    normality_stats <- map_df(names(numeric_vars), function(var) {
      values <- data[[var]]
      # Calculate normality statistics for the current variable
      calculate_normality_stats(values)
    })
    
    result <- bind_cols(result, normality_stats)
  }
  
  
  # Reorder columns if necessary so that Variable is always the first column
  result <- result %>%
    select(Variable, everything())
  
  # Print plain table in the console with only 3 decimal places
  print(result, digits = 3)
  

  
  # Create gt table with values at only 3 decimal places and add title
  result_table <- gt(result, auto_align = TRUE) %>%
    tab_header(title = title) %>%
    fmt_number(
      columns = n,
      decimals = 0  # Set decimals to 0 for the "n" column
    ) %>%
    fmt_number(
      columns = setdiff(names(result), "n"),  # Exclude "n" column
      decimals = 3
    ) %>% 
    cols_align("left", columns = "Variable") %>% 
    cols_align("center", columns = setdiff(names(result),"Variable")) %>% 
    cols_align_decimal(columns = everything()) 
  
  # Set tab spanners for selected labels
  if ("Quantiles" %in% selected_labels) {
    result_table <- result_table %>%
      tab_spanner(
        label = "Quantiles",  # Use the provided title here
        columns = c("Median", "IQR", "IQR_Range")
      ) %>%
      fmt_number(
        columns = c("Median", "IQR"),
        decimals = 2
      ) %>%
      fmt_markdown(columns = "IQR_Range")  # Format IQR_Range as markdown
  }
  
  
  
  if ("Percentiles" %in% selected_labels) {
    result_table <- result_table %>%
      tab_spanner(
        label = "Percentiles",
        columns = starts_with("p")
      )
  }
  
  if ("Normality" %in% selected_labels) {
    result_table <- result_table %>%
      tab_spanner(
        label = "Normality",
        columns = c("Skewness", "Skewness_Type", "Kurtosis", "Kurtosis_Type")
      )
  }
  
  # Set the table title
  result_table <- result_table %>%
    tab_header(title = title) 
  
  # Return the tibble
  return(result_table)
  
}

#' @title Calculate Advanced Statistics for Categorical Variables
#'
#' @description
#' This function calculates advanced statistics for categorical variables.
#'
#' @param data A data frame.
#' @param title An optional title for the gt table.
#'
#' @return A gt table with advanced statistics for categorical variables.
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(tidyverse)
#' library(gt)
#'
#' # Load the data frame
#' df <- read_csv("path/to/your/data.csv")
#'
#' # Calculate advanced statistics for categorical variables
#' calc_cat_stats(df, title = "Categorical Variable Statistics")
#' }
#'
#' @export
#' @importFrom gt gt fmt_number 
#' @title Calculate Advanced Statistics for Categorical Variables
#'
#' @description
#' This function calculates advanced statistics for categorical variables.
#'
#' @param data A data frame.
#' @param title An optional title for the gt table.
#'
#' @return A gt table with advanced statistics for categorical variables.
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(tidyverse)
#' library(gt)
#'
#' # Load the data frame
#' df <- read_csv("path/to/your/data.csv")
#'
#' # Calculate advanced statistics for categorical variables
#' calc_cat_stats(df, title = "Categorical Variable Statistics")
#' }
#'
#' @export
#' @importFrom gt gt fmt_number 
calc_cat_stats <- function(data, title = "Categorical Statistics") {
  library(gt)
  library(dplyr)
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Select non-ID and non-numeric variables
  categorical_vars <- data %>%
    select(-ID) %>%
    select(where(~ !is.numeric(.)))
  
  result <- map_df(names(categorical_vars), function(var) {
    values <- data[[var]]
    categories <- table(values, useNA = "ifany")
    
    tibble(
      Variable = var,
      Levels = names(categories),
      UniqueValues = n_distinct(values, na.rm = TRUE),
      Frequencies = as.list(categories),
      Proportions = scales::percent(as.numeric(categories) / sum(categories), scale = 100)
    )
  }) 
  
  # Group by row when variables are split by level
  result_table <- gt(result, auto_align = TRUE) %>%
    tab_header(title = title) %>%
    fmt_number(
      columns = "UniqueValues",
      decimals = 0  # Set decimals to 0 for the "n" column
    ) %>%
    fmt_number(
      columns = setdiff(names(result), "UniqueValues"),  # Exclude "UniqueValues" column
      decimals = 2
    ) %>% 
    cols_align("center", 
               columns = setdiff(names(result),"Levels")) %>% 
    cols_align_decimal(
      columns = "Proportions"
    ) 
  
  # Set the default title and group rows
  result_table <- result_table %>%
    tab_header(
      title = title
    ) 
  
  # Print in console
  print(result)
  
  # Return gt HTML table
  return(result_table)
}

