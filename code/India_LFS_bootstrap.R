    # This script takes the processed data from the India 2011 LFS survey and calculates variance using bootstrap 
    # Note: This script is expected to be run in the HelpAge/data folder (same location as the script)
    
    # Load packages
    require(dplyr)
    require(boot)
    require(purrr)
    
    # function to calculate mean using weights that works with boot() function
    boot_mean = function(df, i){
      d2 = df[i,]
      return(sum(d2$weekly_earnings * d2$weight) / sum(d2$weight))
    }
    
    # Set seed for reproducibility
    set.seed(1104)
    
    # Load data
    df = read.csv("../data/processed/2011_India_LFS_individuals.csv")
    employed = filter(df, employment_status == "Employed")
    
    # Performs bootstrap to calculate variance of income of all groups of employed people
    # for each of the different analysis groups 
    # Note: Uses 1000 replicates
    bootstrap_by_group = function(analysis_group){
      # Bootstrap by group
      results = setNames(unique(employed[,analysis_group]), unique(employed[,analysis_group])) %>% 
        map(function(group) {
          d2analyze = employed[employed[,analysis_group] == group, ]
          b2 = boot(data = d2analyze, statistic = boot_mean, R = 1000)
          out = c(levels(employed[,analysis_group])[group], nrow(d2analyze), b2$t0, var(b2$t), var(d2analyze$weekly_earnings) / nrow(d2analyze))
          return(out)
          })
    
      # Get results in a nice format
      results_df = as.data.frame(t(as.data.frame(results)), stringsAsFactors = FALSE) %>%
        rename(group = V1, n = V2, estimate = V3, bootstrap_variance = V4, SRS_variance = V5) %>%
        mutate(n = as.numeric(n), estimate = as.numeric(estimate), bootstrap_variance = as.numeric(bootstrap_variance), SRS_variance = as.numeric(SRS_variance)) %>%
        mutate(bootstrap_SE = sqrt(bootstrap_variance))
      rownames(results_df) = NULL
      results_df = arrange(results_df, desc(n))
    
      return(results_df)
    }
    
    # Run bootstrap for each of our 3 analysis groups
    results1 = bootstrap_by_group("analysis_group1")
    results2 = bootstrap_by_group("analysis_group2")
    results3 = bootstrap_by_group("analysis_group3")
    
    # Remove "no analysis group", duplicates, and create upper and lower bounds of CI
    results1 = filter(results1, group != "No analysis group")
    results2 = filter(results2, group != "No analysis group")
    results3 = filter(results3, group != "No analysis group")
    results_df = rbind(results1, results2, results3) %>%
      mutate(lower = estimate - (1.96 * bootstrap_SE), upper = estimate + (1.96 * bootstrap_SE)) %>%
      distinct(group, .keep_all = TRUE)

write.csv(results_df, "../data/final/2011_India_LFS_bootstrap_results.csv", row.names = FALSE)