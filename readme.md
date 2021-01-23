# Ukraine
## DHS
**Note: This survey only included people aged 15 - 49, it is being shelved for now**

Data: SAS datasets using the "haven" package to pull it into R

Steps:
- Load all of the individual and household data into R
- Split the data into groups based on all primary levels of disaggregation:
    - Individual: Age, sex, disability
    - Household: Type of household
- For each of the above splits, add a single secondary level of disaggregation:
    - Urban/rural, ethnicity, 
- Then, analyze the sample size needed to get a specified margin of error for the two variables of interest:
    - Individual: Educational attainment by highest level of school attended
    - Household: Proportion of population using safely managed drinking water services
    - Have to either calculate standard error or use the provided standard error adjusted for reduced number of observations in the more disaggregated data
- Also could calculate a 95% confidence interval and report the observed sample size at each level of disaggregation

# India
## LFS

The India periodic labour force survey (PLFS) aims to measure the dynamics in labour force participation and employment status in the interval of 3 months. *It may not be super relevant to older populations.*

It seems that while data is collected about older population, the cutoff for the labour force is 59 years of age.

Data: I think we'll need to use the 2011-2012 dataset unless I can figure out which variables are what columns in the newer dataset