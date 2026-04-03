# Sylvia Hipp
# April 2, 2026
# Conduct clustering analysis to group households
# Aggregate like-households and convert to time series

# Setup Environment -------------------------------------------------------

library(tidyverse)
library(data.table)
library(glue)
library(forecast)
library(future.apply)
library(cluster)

# set wd to access datasets saved locally
setwd("C:/Users/sch90/Documents/Repositories/GualtieriReedHippKastenSalzberg_ENV797_TSA_FinalProject")


# Import Data -------------------------------------------------------------

import_data <- function(filepath){
  # exclude 2011 (lots of missing data + will train on just one year of data)
  df <- fread(filepath) %>% 
    select(-V1) %>% 
    tibble() %>% 
    filter(year(datetime_utc) >= 2012)
  
  # exclude households that have at least cumulative 4 months missing data in the first year
  households_to_exclude <- df %>% 
    mutate(year = year(datetime_utc)) %>% 
    pivot_longer(cols = c(2:ncol(df)), names_to = "household") %>% 
    filter(is.na(value)) %>% 
    group_by(year, household) %>% 
    summarize(n_nas = n()) %>%     # Count hours with missing NAs
    ungroup() %>% 
    filter(n_nas > (24*30*4)) %>%  # missing more than 4 months of data
    distinct(household) %>% pull()
  
  df_filtered <- df %>% 
    select(-all_of(households_to_exclude)) %>% 
    return()
}

lcl_hourly_std <- import_data("data/processed/lcl_data/lcl_hourly_demand_Std.csv")

lcl_hourly_tou <- import_data("data/processed/lcl_data/lcl_hourly_demand_ToU.csv") 


# Clustering --------------------------------------------------------------

## USED CHATGPT FOR HELP

# For purpose of cluster analysis:
# Interpolate missing values using seasonal interpolation (used ChatGPT for help)
fill_seasonal <- function(x){
  x <- as.numeric(x)
  ts_x <- msts(x, seasonal.periods = c(24, 24*7))  # daily and weekly frequency
  return(as.numeric(na.interp(ts_x)))              # interpolate missing values and return numeric vector
}

# identify clusters based on 2012 usage (? - maybe ask luana)
# filter to last 8 months of 2012 to use months with consistent usage from all households
k_means_matrix <- function(x){
  matrix <- x %>% 
    filter(year(datetime_utc) == 2012 & month(datetime_utc) > 4) %>%   # identify clusters just on 2012   
    select(-datetime_utc) %>% 
    as.matrix() %>% 
    future_apply(MARGIN=2, fill_seasonal) %>% 
    t()     # transpose data so row = household, col = time stamp
    
  # normalize household usage so clusters are identified by usage pattern > scale
  matrix_norm <- matrix / rowSums(matrix)
  return(matrix_norm)
}

# Transpose matrices so row = household and col = feature (timestamp)
lcl_matrix_std <- k_means_matrix(lcl_hourly_std)
lcl_matrix_tou <- k_means_matrix(lcl_hourly_tou)

## Perform k-means clustering on data matrices
sil_scores <- sapply(2:10, function(k) {
  km <- kmeans(lcl_matrix_std, centers = k, nstart = 50)
  ss <- silhouette(km$cluster, dist(lcl_matrix_std))
  mean(ss[, 3])
  print(glue("k={k} is done"))
})

plot(2:10, sil_scores, type = "b",
     xlab = "k", ylab = "Average silhouette width")




# Visualize Clusters ------------------------------------------------------


