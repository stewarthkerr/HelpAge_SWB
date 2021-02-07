# This script takes the processed data from the India 2011 LFS survey and creates tables of sample size, estimate, variance, and CI by age, sex, urban and maybe profession
# Note: This script is expected to be run in the HelpAge/data folder (same location as the script)

# Load packages
require(dplyr)

# Load data
df = read.csv("../data/processed/2011_India_LFS_individuals.csv")

# Summarise count by sex, age category, and urban/rural
sex_age5_urban = summarise(group_by(df, sex, age_group5, urban), count = n())
sex_age10_urban = summarise(group_by(df, sex, age_group10, urban), count = n())

# Summarise count by sex, age category, urban/rural, and income
df$income = ifelse(df$weekly_earnings > 0, 1, 0)
sex_age5_urban_income = summarise(group_by(df, sex, age_group5, urban, income), count = n(), average_earnings = mean(weekly_earnings))
sex_age10_urban_income = summarise(group_by(df, sex, age_group10, urban, income), count = n(), average_earnings = mean(weekly_earnings))

# Summarise count by sex, age category, and income
sex_age5_income = summarise(group_by(df, sex, age_group5, income), count = n(), average_earnings = mean(weekly_earnings))
sex_age10_income = summarise(group_by(df, sex, age_group10, income), count = n(), average_earnings = mean(weekly_earnings))


# Summarise count by sex, age category, urban/rural, and disability
df$disability = ifelse(df$Current_Weekly_Activity_Status == "95", 1, 0)
sex_age5_urban_disability = summarise(group_by(df, sex, age_group5, urban, disability), count = n())
sex_age10_urban_disability = summarise(group_by(df, sex, age_group10, urban, disability), count = n())

# Summarise count by sex, age category, and disability
sex_age5_disability = summarise(group_by(df, sex, age_group5, disability), count = n())
sex_age10_disability = summarise(group_by(df, sex, age_group10, disability), count = n())

# Summarise count by sex, age category, labor force/unemployment
sex_age5_employment = summarise(group_by(df, sex, age_group5, employment_status), count = n())
sex_age10_employment = summarise(group_by(df, sex, age_group10, employment_status), count = n())

# Summarise count by sex, age category, and industry
sex_age5_industry = summarise(group_by(df, sex, age_group5, industry), count = n(), average_earnings = mean(weekly_earnings))
sex_age10_industry = summarise(group_by(df, sex, age_group10, industry), count = n(), average_earnings = mean(weekly_earnings)) %>%
  arrange(age_group10, sex, industry)

# Summarise count by sex, age category, and industry for employed people
employed = filter(df, employment_status == "Employed")
employed_sex_age5_industry = summarise(group_by(employed, sex, age_group5, industry), count = n(), average_earnings = mean(weekly_earnings)) %>%
  arrange(age_group5, sex, industry)

# Save tables
write.csv(employed_sex_age5_industry, "../data/final/2011_India_LFS_employed_sex_age5_industry.csv", row.names = FALSE)