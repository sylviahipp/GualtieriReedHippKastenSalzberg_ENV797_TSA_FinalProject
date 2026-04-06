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
library(cowplot)

set.seed(123)   # set seed for clustering 

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


# Prepare data for clustering ---------------------------------------------

## USED CHATGPT FOR HELP

# For purpose of cluster analysis:
# Interpolate missing values using seasonal interpolation
fill_seasonal <- function(x){
  x <- as.numeric(x)
  ts_x <- msts(x, seasonal.periods = c(24, 24*7))  # daily and weekly frequency
  return(as.numeric(na.interp(ts_x)))              # interpolate missing values and return numeric vector
}

# Identify clusters based on 2012 usage
# Filter to last 8 months of 2012 to use months with consistent usage from all households
k_means_matrix <- function(x){
  matrix <- x %>% 
    filter(year(datetime_utc) == 2012 & month(datetime_utc) > 4) %>%   # identify on period where most data is present   
    select(-datetime_utc) %>% 
    as.matrix() %>% 
    future_apply(MARGIN=2, fill_seasonal) %>% 
    t()     # transpose data so row = household, col = time stamp
    
  # normalize household usage so clusters are identified by usage pattern > scale
  matrix_norm <- matrix / rowSums(matrix)
  
  # identify and remove outliers (distance from center above 99 percentile)
  # compute distance from centroid of all households
  centroid <- colMeans(matrix_norm)
  distances <- apply(matrix_norm, 1, function(row) sqrt(sum((row - centroid)^2)))
  outliers <- which(distances > quantile(distances, 0.99))

  matrix_norm_no_outliers <- matrix_norm[-outliers,]

  return(matrix_norm_no_outliers)
}

lcl_matrix_std <- k_means_matrix(lcl_hourly_std)  
lcl_matrix_tou <- k_means_matrix(lcl_hourly_tou)

# number of households
print(glue("Standard Households in sample: {lcl_matrix_std %>% nrow()}"))
print(glue("Time-of-Use Households in sample: {lcl_matrix_tou %>% nrow()}"))

# Find optimal clusters for each group ------------------------------------

# sil_scores_std <- sapply(2:5, function(k) {
#   print(glue("starting std k={k}"))
#   km <- kmeans(lcl_matrix_std, centers = k, nstart = 10)
#   ss <- silhouette(km$cluster, dist(lcl_matrix_std))
# 
#   print(glue("std k={k} is done"))
#   return(mean(ss[, 3]))
# })
# 
# sil_scores_tou <- sapply(2:5, function(k) {
#   print(glue("starting tou k={k}"))
#   km <- kmeans(lcl_matrix_tou, centers = k, nstart = 10)
#   ss <- silhouette(km$cluster, dist(lcl_matrix_tou))
#   
#   print(glue("tou k={k} is done"))
#   return(mean(ss[, 3]))
# })
# 
# plot(2:5, sil_scores_std, type = "b",
#      xlab = "k - std", ylab = "Average silhouette width")   # k=2 has maximum silhouette
# 
# plot(2:5, sil_scores_tou, type = "b",
#      xlab = "k - tou", ylab = "Average silhouette width")   # k=2 has maximum silhouette

# pick k with maximum silhouette scores


# Perform clustering ------------------------------------------------------

get_clusters <- function(matrix, k){
  households <- rownames(matrix)
  km <- kmeans(matrix, centers = k, nstart = 10)
  
  cluster_df <- tibble(
    household = households, 
    cluster = km$cluster
  )
  
  # visualize clusters
  cluster_means <- aggregate(matrix,
                             by = list(cluster = km$cluster),
                             FUN = mean)

  matplot(t(cluster_means[,-1]), type = "l", lty = 1,
          main = glue("k = {k} cluster profiles"))
  
  return(cluster_df)
}

k_std <- 2
k_tou <- 2

std_clusters <- get_clusters(lcl_matrix_std, k = k_std)
tou_clusters <- get_clusters(lcl_matrix_tou, k = k_tou)

## Investigate some strange patterns in data (high regular peaks around midnight)
test_std <- lcl_hourly_std %>% 
  pivot_longer(cols = c(2:ncol(.)), 
               names_to = "household", 
               values_to = "load_kwh") %>% 
  inner_join(std_clusters, by = "household") %>% 
  mutate(cluster = factor(cluster)) %>% 
  filter(cluster == 1)



# Aggregate households by cluster -----------------------------------------

# identify households by clusters identified in k-means clustering 
# aggregate average load per hour across the cluster
aggregate_clusters <- function(df, clusters){
  
  df_clusters <- df %>% 
    pivot_longer(cols = c(2:ncol(df)), 
                 names_to = "household", 
                 values_to = "load_kwh") %>% 
    # inner join to exclude outliers identified during k-means analysis
    inner_join(clusters, by = "household") %>% 
    mutate(cluster = factor(cluster))
  
  # also get some summary of how many households are in each cluster
  
  df_agg <- df_clusters %>% 
    # group by cluster and hour and take mean
    group_by(datetime_utc, cluster) %>% 
    summarize(load_kwh = mean(load_kwh, na.rm = TRUE)) %>%   # remove NAs for those that had NAs earlier in the year
    ungroup()
  
  # ggplot(df_agg, aes(x = datetime_utc, y = load_kwh, color = cluster)) + 
  #   geom_line
  
  return(df_agg)
}

std_aggregated <- aggregate_clusters(lcl_hourly_std, std_clusters)
tou_aggregated <- aggregate_clusters(lcl_hourly_tou, tou_clusters)

print(glue("Std: \nHouseholds in Cluster 1: {std_clusters %>% filter(cluster==1) %>% nrow()} \nHouseholds in Cluser 2: {std_clusters %>% filter(cluster==2) %>% nrow()}"))
print(glue("ToU: \nHouseholds in Cluster 1: {tou_clusters %>% filter(cluster==1) %>% nrow()} \nHouseholds in Cluser 2: {tou_clusters %>% filter(cluster==2) %>% nrow()}"))

write_csv(std_aggregated, "data/processed/Std_household_load.csv")
write_csv(tou_aggregated, "data/processed/ToU_household_load.csv")

# Visualize Clusters ------------------------------------------------------

# smooth to daily 
plot_clusters <- function(df_agg, group="Std"){
  if(group=="Std"){
    colors = c("orange", "red")
  }
  else{
    colors = c("darkgreen", "darkblue")
  }
  
  df_agg_daily <- df_agg %>% 
    mutate(date = as.Date(datetime_utc), 
           cluster = factor(cluster, 
                            levels=c(1,2), 
                            labels=c(glue("{group} Cluster 1"), glue("{group} Cluster 2")))) %>% 
    group_by(date, cluster) %>% 
    summarize(daily_load = sum(load_kwh)) %>% 
    ungroup() 
  
  plot <- ggplot(df_agg_daily) + 
    geom_line(aes(x = date, y = daily_load, color = cluster), 
              linewidth = 0.25) + 
    geom_vline(aes(xintercept = as.Date("2013-01-01")), 
               color = "black", linetype = 2) + 
    scale_x_date(date_labels = "%b-%Y") +
    scale_color_manual(values = colors) +
    theme_classic() + 
    theme(legend.position = "right") +
    labs(
      x = NULL, 
      y = "Daily Load (kWh)", 
      color = NULL, 
      title = group
    )
    
  return(plot)
}

std_cluster_plot <- plot_clusters(std_aggregated, group="Std")
tou_cluster_plot <- plot_clusters(tou_aggregated, group="ToU")

plot_grid(std_cluster_plot, tou_cluster_plot, 
          nrow = 2) %>% 
  ggsave(filename = "outputs/daily_load_by_cluster.png", 
         width = 5, height = 3.5)



# km2 <- kmeans(lcl_matrix_tou, centers = 2, nstart = 50)
# km3 <- kmeans(lcl_matrix_tou, centers = 3, nstart = 50)
# km4 <- kmeans(lcl_matrix_tou, centers = 4, nstart = 50)
# 
# table(km2$cluster)
# table(km3$cluster)
# table(km4$cluster)
# 
# cluster_means <- aggregate(lcl_matrix_tou,
#                            by = list(cluster = km3$cluster),
#                            FUN = mean)
# 
# matplot(t(cluster_means[,-1]), type = "l", lty = 1,
#         main = "k = 2 cluster profiles")

