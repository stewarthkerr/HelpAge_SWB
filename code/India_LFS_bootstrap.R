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
    
    # Define the groups we will include in the table and will need to put into bootstrap function
    industry_groups = c("Farming, forestry, or fishing", "Manufacturing", "Construction", "Manufacturing or construction", "Other", "Nonfarming", "All")
    geo_groups = c("Urban", "Rural", "Urban and Rural")
    age_groups = c("Under 60", "60-64", "65-69", "60-69", "70-74", "75-79", "70-79", "70+", "75+", "80+")
    sex_groups = c("Male", "Female")
    analysis_groups = expand.grid(industry = industry_groups, geo = geo_groups, age_group = age_groups, sex = sex_groups, stringsAsFactors = FALSE)
    # Collapse groups that we won't estimate (because sample size is too small)
    ### Removing urban/rural classification for older than 60-64
    analysis_groups = filter(analysis_groups, !(((geo == "Urban") | (geo == "Rural")) & (!(age_group %in% c("Under 60", "60-64", "60-69")))))
    ### Keeping "manufacturing or construction" category only for 60-64, 65-69, 60-69 age groups 
    analysis_groups = filter(analysis_groups, !(industry == "Manufacturing or construction" & !(age_group %in% c("60-64", "65-69", "60-69"))))
    ### For 70+, 70-74, 75-79, keep only "Farming, forestry, or fishing", "Nonfarming", or "All"
    analysis_groups = filter(analysis_groups, !(age_group %in% c("70+", "70-74", "75-79", "70-79") & industry %in% c("Manufacturing", "Construction", "Manufacturing or construction", "Other")))
    ### For 75+, 80+ keep only "all" group
    analysis_groups = filter(analysis_groups, !(age_group %in% c("75+", "80+") & industry != "All"))
    ### Drop "nonfarming" for all ages except 70+, 70-74, 75-79, and 70-79
    analysis_groups = filter(analysis_groups, !(industry == "Nonfarming" & (age_group %in% c("Under 60", "60-64", "65-69", "60-69", "75+", "80+"))))
    
    # Function to filter by analysis group
    ### Takes a row of analysis_groups and filters individuals df to contain only those people
    filter_by_ag = function(ag_row){
      industry_filter = case_when(
        ag_row$industry == "Manufacturing or construction" ~ c("Manufacturing", "Construction", "", ""),
        ag_row$industry == "Nonfarming" ~ c("Manufacturing", "Construction", "Other", ""),
        ag_row$industry == "All" ~ c("Manufacturing", "Construction", "Other", "Farming, forestry, or fishing"),
        TRUE ~ c(ag_row$industry)
        )
      
      age_min = case_when(
        ag_row$age_group == "Under 60" ~ 0,
        ag_row$age_group %in% c("60-64", "60-69") ~ 60,
        ag_row$age_group == "65-69" ~ 65,
        ag_row$age_group %in% c("70+", "70-74", "70-79") ~ 70,
        ag_row$age_group == "75" ~ 75,
        TRUE ~ 80
        )
      
      age_max = case_when(
        ag_row$age_group == "Under 60" ~ 59,
        ag_row$age_group == "60-64" ~ 64,
        ag_row$age_group %in% c("60-69", "65-69") ~ 69,
        ag_row$age_group == "70-74" ~ 74,
        ag_row$age_group %in% c("70-79", "75-79") ~ 79,
        TRUE ~ 9999
        )
      
      geo_filter = case_when(
        ag_row$geo == "Urban and Rural" ~ c(1, 0),
        ag_row$geo == "Urban" ~ c(1, -1),
        TRUE ~ c(0, -1)
        )
      
      # Perform the filter
      out = filter(employed, industry %in% industry_filter, age >= age_min, age <= age_max, sex == ag_row$sex, urban %in% geo_filter)
      return(out)
    }
    
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