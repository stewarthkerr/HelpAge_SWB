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
#b51 = read_sav("../data/raw/India/LFS/2011/Block_5_1_Usual principal activity particulars of household members.sav")
b53 = read_sav("../data/raw/India/LFS/2011/Block_5_3_Time disposition during the week ended on .sav")

# Create a unique ID for each individual by combining FSU, stratum, sub-stratum, hamlet group, second stage stratum, householdr, and individual serial numbers
LFS_ID = function(LFS_HH_df){
  out = paste0("FSU", LFS_HH_df$FSU_Serial_No, "S", LFS_HH_df$Stratum, "SS", LFS_HH_df$Sub_Stratum_No, "HG", LFS_HH_df$Hamlet_Group_Sub_Block_No, "SSS", LFS_HH_df$Second_Stage_Stratum_No, "HH", LFS_HH_df$Sample_Hhld_No, "P", LFS_HH_df$Person_Serial_No)
  return(out)
}
b4$ID = LFS_ID(b4)
#b51$ID = LFS_ID(b51)
b53$ID = LFS_ID(b53)

# Subset data to only those in the labor force
# India 2011 LFS definition of labor force:
### Everyone except those who have current_weekly_activity_status of:
##### Attended educational institution (91), attended domestic duties only (92), attended domestic duties and ... (93),
##### rentiers, pensioners, remittance recipients, etc. (94), not able to work due to disability (95), others (97), children, aged 0-4 (99)
b53_lf = filter(b51, !(Current_Weekly_Activity_Status %in% c("91","92","93","94","95","97","99")))
b4_lf = filter(b4, ID %in% b53_lf$ID)
#b51_lf = filter(b53, ID %in% b53_lf$ID)

# rm full datasets because they're very large and I need space
rm(b4, b53)

# Keep only the variables we care about
# NOTE: There are a few other multipliers, not sure which we should keep
b4_lf = select(b4_lf, ID, FSU_Serial_No, Stratum, Sub_Stratum_No, Hamlet_Group_Sub_Block_No, Second_Stage_Stratum_No, Sample_Hhld_No, Person_Serial_No, Sex, Age, b4_Multiplier = Multiplier_comb)
b53_lf = select(b53_lf, ID, Current_Weekly_Activity_Status, Current_Weekly_Activity_NIC_2008, Wage_and_Salary_Earnings_Total, b53_Multiplier = Multiplier_comb)

# In b53_lf, there can be multiple rows per person if they did different jobs during the week
# Take the earnings as the sum over the week and the occupation as either the most frequent or the one they received most compensation for