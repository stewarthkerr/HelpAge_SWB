# This script takes the processed data from the India 2011 LFS survey and calculates variance using bootstrap 
# Note: This script is expected to be run in the HelpAge/data folder (same location as the script)

# Load packages
require(dplyr)
require(boot)
require(purrr)

# function to calculate mean that works with boot() function
boot_mean = function(df, i){
  d2 = df[i,]
  return(sum(d2$weekly_earnings) / nrow(d2))
}

# Set seed for reproducibility
set.seed(1104)

# Load data
df = read.csv("../data/processed/2011_India_LFS_individuals.csv")
employed = filter(df, employment_status == "Employed")

# creates a variable with unique name based on unique grouping
employed$analysis_group = paste(employed$sex, employed$age_group5, employed$industry, sep = "; ")

# Performs bootstrap to calculate variance for all groups
z = setNames(unique(employed$analysis_group), unique(employed$analysis_group)) %>% 
  map(function(group) {
    d2analyze = employed[employed$analysis_group == group, ]
    b2 = boot(data = d2analyze, statistic = boot_mean, R = 100)
    out = c(nrow(d2analyze), b2$t0, var(b2$t))
    return(out)
    })

  
# TODO: Generalize this code