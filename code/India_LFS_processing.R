# This script loads data from the India 2011 LFS survey and processes it
# Data files from the survey which are relevant to the target population
### and HelpAge's analysis of interest are:
##### Block_4_Demographic particulars of household members
##### Block_5_1_Usual principal activity particulars of household members
##### Block_5_3_Time disposition during the week ended on
# Note: This script is expected to be run in the HelpAge/data folder (same location as the script)

# Load packages
require(dplyr)
require(haven)

# Load data
b4 = read_sav("../data/raw/India/LFS/2011/Block_4_Demographic particulars of household members.sav")
b53 = read_sav("../data/raw/India/LFS/2011/Block_5_3_Time disposition during the week ended on .sav")

# Create a unique ID for each individual by combining FSU, stratum, sub-stratum, hamlet group, second stage stratum, householdr, and individual serial numbers
LFS_ID = function(LFS_HH_df){
  out = paste0("FSU", LFS_HH_df$FSU_Serial_No, "S", LFS_HH_df$Stratum, "SS", LFS_HH_df$Sub_Stratum_No, "HG", LFS_HH_df$Hamlet_Group_Sub_Block_No, "SSS", LFS_HH_df$Second_Stage_Stratum_No, "HH", LFS_HH_df$Sample_Hhld_No, "P", LFS_HH_df$Person_Serial_No)
  return(out)
}
b4$ID = LFS_ID(b4)
b53$ID = LFS_ID(b53)

# Create employment status variable
# India 2011 LFS definition of labor force:
### Everyone except those who have current_weekly_activity_status of:
##### Attended educational institution (91), attended domestic duties only (92), attended domestic duties and ... (93),
##### rentiers, pensioners, remittance recipients, etc. (94), not able to work due to disability (95), others (97), children, aged 0-4 (99)
b53 = mutate(b53, employment_status = case_when(
  Current_Weekly_Activity_Status %in% c("81","82") ~ "Unemployed",
  Current_Weekly_Activity_Status %in% c("91","92","93","94","95","97","99") ~ "Not in labor force",
  TRUE ~ "Employed"
))
b53$labor_force = ifelse(b53$Current_Weekly_Activity_Status %in% c("91","92","93","94","95","97","99"), 0, 1)
b53$unemployed = ifelse(b53$Current_Weekly_Activity_Status %in% c("81","82"), 1, 0)

# Calculate earnings
### In b53_lf, there can be multiple rows per person if they did different jobs during the week
### Sum earnings by person across all days of the week then join back to b53
b53_earnings = summarise(group_by(b53, ID), weekly_earnings = sum(Wage_and_Salary_Earnings_Total, na.rm = TRUE))

# Derive age group and translate sex from numbers to words
### Doing groups of 5 and 10
b4 = mutate(b4, age_group5 = case_when(
  Age <= 59 ~ "Under 60",
  Age < 65 ~ "60-64",
  Age < 70 ~ "65-69",
  Age < 75 ~ "70-74",
  Age < 80 ~ "75-79",
  Age >= 80 ~ "80+"
  ), age_group10 = case_when(
    Age <= 59 ~ "Under 60",
    Age < 70 ~ "60-69",
    Age < 80 ~ "70-79",
    Age < 90 ~ "80-89",
    Age >= 90 ~ "90+"
  ),
  sex = ifelse(Sex == "1", "Male", "Female"))

# Derive occupation 
### There are many different occupation categories, collapse into a small number of groups
### NOTE: Because this is the activity performed during the previous week, it may not be accurated to call it profession
b53 = mutate(b53, urban = ifelse(Sector == "2", 1, 0), industry = case_when(
  employment_status %in% c("Not in labor force", "Unemployed") ~ "None",
  substring(Current_Weekly_Activity_NIC_2008, 1, 2) %in% c("01","02","03") ~ "Farming, forestry, or fishing",
  substring(Current_Weekly_Activity_NIC_2008, 1, 2) %in% c("10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32") ~ "Manufacturing",
  substring(Current_Weekly_Activity_NIC_2008, 1, 2) %in% c("41","42","43") ~ "Construction",
  #substring(Current_Weekly_Activity_NIC_2008, 1, 2) %in% c("45","46","47") ~ "Trade",
  #substring(Current_Weekly_Activity_NIC_2008, 1, 2) %in% c("84","85") ~ "Public administration or education",
  #substring(Current_Weekly_Activity_NIC_2008, 1, 2) %in% c("49","50","51","52","53") ~ "Transportation",
  !is.na(Current_Weekly_Activity_NIC_2008) ~ "Other"
  ))

# Combine datasets and keep only the variables we care about
# NOTE: There are a few other multipliers, not sure which we should keep
b4 = select(b4, ID, FSU_Serial_No, Stratum, Sub_Stratum_No, Hamlet_Group_Sub_Block_No, Second_Stage_Stratum_No, Sample_Hhld_No, Person_Serial_No, sex, Age, age_group5, age_group10, Multiplier_comb)
b53 = select(b53, ID, Current_Weekly_Activity_Status, Current_Weekly_Activity_NIC_2008, urban, employment_status, industry) %>%
  distinct()
out = left_join(b4, left_join(b53, b53_earnings, by = "ID"), by = "ID")

# Save results
write.csv(out, "../data/processed/2011_India_LFS_individuals.csv", na = "", row.names = FALSE)
