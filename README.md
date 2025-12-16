Please cite as: Quijada et. al.,Dengue Epidemiology and Transmission Intensity across Panama during 2000-2024
 
 Available as a pre-print (not peer-reviewed) at SSRN: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5461361%20


# Repository structure
This repository contains: 


##Data 
- Line-List 2000-2024: Contains epidemiological variables by age, region, and sex, used primarily for delay analyses.
- Dengue Cases: Includes reported cases disaggregated into nine age groups, sex, region, and district. This dataset was used to estimate constant-in-time transmission metrics and for logistic regression analyses. It also includes data on deaths and hospitalizations.
- PopRegion 2000-2024: Provides regional-level population data disaggregated by age group, sex, and year. This dataset was used to compute constant-in-time estimates, incidence rates, and logistic regression models.
- PopDistrict 2000-2024: Provides district-level population data disaggregated by age group, sex, and year. This dataset was used to compute constant-in-time estimates, incidence rates, and logistic regression models.




##StanCode
- Codes to estimate dengue force of infection from age-specific reported case data by Region and district-level.




##Scripts
The scripts files contains the main scripts for each separate analysis:
- Constant-in-Time by District is the main script to estimate the force of infection at district-level.
- Constant-in-Time by Region is the main script to estimate the force of infection at region-level.
- Multivariate Logistic Regression is the main script to compute the RRs by reported Indicence, Hospitalised and Fatality.
- Delay Analyses is the script producing the estimates of epidemiological delays in dengue reported at national and regional-level.



