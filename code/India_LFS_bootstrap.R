# This script takes the processed data from the India 2011 LFS survey and calculates variance using bootstrap 
# Note: This script is expected to be run in the HelpAge/data folder (same location as the script)

# Load packages
require(dplyr)
require(boot)
require(purrr)

# function to calculate mean using weights that works with boot() function
boot_mean = function(df, i){
  d2 = df[i,]
  return(sum(d2$weekly_earnings * d2$Multiplier_comb) / sum(d2$Multiplier_comb))
}

# Set seed for reproducibility
set.seed(1104)

# Load data
df = read.csv("../data/processed/2011_India_LFS_individuals.csv")
employed = filter(df, employment_status == "Employed")

# Performs bootstrap to calculate variance of income of all groups of employed people
# Note: Uses 1000 replicates
results = setNames(unique(employed$analysis_group), unique(employed$analysis_group)) %>% 
  map(function(group) {
    d2analyze = employed[employed$analysis_group == group, ]
    b2 = boot(data = d2analyze, statistic = boot_mean, R = 1000)
    out = c(levels(employed$analysis_group)[group], nrow(d2analyze), b2$t0, var(b2$t), var(d2analyze$weekly_earnings) / nrow(d2analyze))
    return(out)
    })

# Get results in a nice format
results_df = as.data.frame(t(as.data.frame(results)), stringsAsFactors = FALSE) %>%
  rename(group = V1, n = V2, estimate = V3, bootstrap_variance = V4, SRS_variance = V5) %>%
  mutate(n = as.numeric(n), estimate = as.numeric(estimate), bootstrap_variance = as.numeric(bootstrap_variance), SRS_variance = as.numeric(SRS_variance)) %>%
  mutate(design_effect_variance = 3 * SRS_variance)
rownames(results_df) = NULL  

# Save results
results_df = arrange(results_df, desc(bootstrap_variance))
write.csv(results_df, "../data/final/2011_India_LFS_bootstrap_results.csv", row.names = FALSE)