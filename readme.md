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

We use the 2011-2012 dataset because it has better documentation than the provided more recent dataset. 

We seek to understand the average hourly earnings of employees by sex, age, occuptation, and disability status. These characteristics are captured in the following variables:
* Hourly earnings - Wage and salary earnings (in Rs) for the week prior to the subject being interviewed
* Sex: Male (51.2%), Female (48.8%)
* Age: 0 - 110, mean = 29.4
* Occupation: NIC-2008 2-digit code. There are about 99 different occupations. We choose to group by crop and animal production, hunting and related services (code 01) vs. construction (code 41), retail trade (code 47), land transport (code 49), public administration (code 84), education (code 85), and other (everything else)
* Disability status: The only information I can find about disability status is "not able to work due to disability". Earnings information is not collected for these people.

The survey defines the labor force as everyone except those who have current_weekly_activity_status of:
* Attended educational institution (91)
* Attended domestic duties only (92)
* Attended domestic duties and ... (93)
* Rentiers, pensioners, remittance recipients, etc. (94), 
* Not able to work due to disability (95), 
* Others (97)
* Did not work due to temporary sickness (for casual workers only)
* Children, aged 0-4 (99)
Earnings data was only collected for those people in the labor force. Because earnings data was only collected as the total earned in the past 7 days, we choose to activity status in the past 7 days to determine labor force participation.
