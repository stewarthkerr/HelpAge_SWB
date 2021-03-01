# India LFS Introduction
HelpAge International wants to challenge established norms for statistical reporting on older persons by proving that data disaggregation to a lower, more granular level is possible and statistically robust. Nongranular statistics reinforces an oversimplified picture of inequalities and the inadequate data itself becomes a barrier to the inclusion of at-risk and marginalized groups in policy and program responses. 

To serve this goal, we analyzed data from the employment and unemployment surveys included in the 2011 India National Sample Survey (NSS)  to determine the lowest level of disaggregation that was possible while maintaining statistical robust estimates of average weekly earnings in rupees. The employment and unemployment surveys of the NSS aim to get estimates of various employment characteristics at the national and state level. In addition to employment related variables, individual characteristics such as region, age, sex, industry, education, and others are collected by the survey. In accordance with HelpAge's statement of work, our analysis focuses on sample size differences across varied groupings of age, sex, employment industry, and region (urban/rural) stratifiers. Additionally, we also provide preliminary findings on how average weekly earnings vary across these groupings. Disability status was only collected in relationship to employment (i.e. unable to work due to disability) and was not available to analyze in relation to average weekly earnings.

In our analysis, we sought to answer three specific research questions related to data disaggregation:

1. What is the most granular level of disaggregation of age, sex, and employment industry? What are the most appropriate age bands (i.e. 5 year groupings or 10 year groupings) and upper age cohort (i.e. 80+, 85+, etc)? How does sample size differ going from broader to more granular disaggregation?
2. What is the most granular level of disaggregation of age, sex, and employment industry when we also include geographic location (urban/rural)?
3. Based on these results, what general recommendations or considerations can be made on data disaggregation for similar surveys?

For a detailed writeup of our analysis, see `India_LFS_report.pdf`.

# Code
The code for this project lives in the code folder. The scripts used include:

* `code/India_LFS_processing.R` - Takes the data extracted from the 2011 India NSS survey used Nesstar and processes the data to generate the file `data/processed/2011_India_LFS_individuals.csv`
* `code/India_LFS_bootstrap.R` - takes `data/processed/2011_India_LFS_individuals.csv` and uses the bootstrap procedure to calculate 90% confidence intervals for average weekly earnings by various groupings. Results are saved to `data/final/2011_India_LFS_bootstrap_results.csv`
* `code/India_LFS_tables.R` - takes `data/processed/2011_India_LFS_individuals.csv` and creates frequency tables for various groupings
* `India_LFS_report.Rmd` - takes `data/processed/2011_India_LFS_individuals.csv` and `data/final/2011_India_LFS_bootstrap_results.csv` and generates the final report provided to HelpAge