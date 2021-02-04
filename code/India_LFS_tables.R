# This script takes the processed data from the India 2011 LFS survey and creates tables of sample size, estimate, variance, and CI by age, sex, urban and maybe profession
# Note: This script is expected to be run in the HelpAge/data folder (same location as the script)

# Load packages
require(dplyr)

# Load data
df = read.csv("../data/processed/2011_India_LFS_individuals.csv")

# Summarise count by sex, age category, and urban/rural
sex_age5_urban = summarise(group_by(df, sex, age_group5, urban), count = n())
sex_age10_urban = summarise(group_by(df, sex, age_group10, urban), count = n())
