

#Multivariate logistic regression models for dengue: incidence, hospitalisation, and fatality 
# Panama, 2000–2024

# Cite as:                                                                                
# Quijada M et al, Dengue Epidemiology and Transmission Intensity across Panama           
# during 2000-2024                                                                        
# Pre-print available at: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5461361%20  

# Outputs:
#  - Adjusted IRR for reported incidence (Poisson with offset)
#  - Adjusted RR for hospitalisation among cases
#  - Adjusted RR for case fatality among cases


# Load Required Libraries
library(dplyr)
library(ggplot2)
library("glm2")
library("broom")
library(stats)
library(Hmisc)
library(writexl)
theme_set(theme_classic())


# Read data (RDS)
Cases <- readRDS("path/to/DengueCases.rds") #Dengue Cases by Region and District
Pop <- readRDS("path/to/PopRegion2000_24.rds") #Population by Region and District
Dead  <- readRDS("path/to/Dead.rds") #Cases Fatality
Hospitalised <- readRDS("path/to/Hospitalised.rds") #Hospitalised Cases


#IRR for reported incidence
#Cases
# Convert wide Year columns into long format
# Each Year becomes a category under "Year"
# The corresponding case counts are placed in the "Cases" column
Casescolumn <- Cases %>%
  pivot_longer(cols = matches("^\\d{4}$"),   # new column with the Year
               names_to = "Year", values_to = "Cases",  # new column with the case counts
names_transform = list(Year = as.integer))  # <-- ensures Year is integer


# Aggregate to Year x Age-group x Sex
CasesT <- Casescolumn  %>%
  group_by(`Age-group`, Year, Sex) %>%
  dplyr::summarize(Cases = sum(Cases, na.rm = TRUE))


#Population
# Convert wide age-band columns into long format
# Each age-band becomes a category under "Age-group"
# The corresponding case counts are placed in the "Cases" column
Pop <- Pop %>%
  pivot_longer(cols = c(`0-9`, `10-19`, `20-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `over80`), 
               names_to = "Age-group", values_to = "Pop")


#Population Data 
PopT <- Pop %>%
  group_by(`Age-group`, Year, Sex) %>%
  dplyr::summarize(Popt = sum(Pop, na.rm = TRUE))


#Merge Cases and Population 
Incage <- left_join(CasesT, PopT,by = c("Year", "Age-group", "Sex"))

#multivariate Logistic Regression 
IncidenceModel <- glm(Cases ~ `Age-group` + Sex + offset(log(Popt)),
                      family = poisson (link = "log"), #log incidence rate ratios (IRR)
                      data =Incage)

#Credible intervals 
IncModel <- tidy(IncidenceModel, exponentiate = TRUE, conf.int = TRUE) %>%
dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

# Save results
write_xlsx(IncModel, "~/path/to/your_file.xlsx")





#HRR Hospitalisation among cases
# Convert wide Year columns into long format
# Each Year becomes a category under "Year"
# The corresponding hospitalised counts are placed in the "Hosp" column
Hospitalisedcolumn <- Hospitalised %>%
  pivot_longer(cols = matches("^\\d{4}$"),   # new column with the Year
               names_to = "Year", values_to = "Hosp",  # new column with the case counts
               names_transform = list(Year = as.integer))  # <-- ensures Year is integer


# Hospitalised/Aggregate to Year x Age-group x Sex
Hosp <- Hospitalisedcolumn %>%
  group_by(Year, Sex,`Age-group`) %>%
  summarise(Hosp = n(), .groups = "drop")


#Merge Hospitalised and Cases
rateHS <- left_join(CasesT, Hosp, by = c("Sex", "Year", "Age-group")) 


# Ensure no negative failures and valid counts
rateHS <- rateHS %>%
  filter(!is.na(Hosp), !is.na(Cases), Cases > 0, Hosp >= 0, Hosp <= Cases)


#multivariate Logistic Regression 
model_Hosp <- glm(cbind(Hosp, Cases - Hosp) ~ Sex + `Age-group`,
                  family = binomial(link = "log"),
                  data = rateHS)

#Credible intervals 
model_Hosp <- tidy(model_Hosp, exponentiate = TRUE, conf.int = TRUE) %>%
mutate(across(where(is.numeric), round, 2))

# Save results
write_xlsx(model_Hosp, "~/path/to/your_file.xlsx")




##FRR Cases Fatality among cases
# Convert wide Year columns into long format
# Each Year becomes a category under "Year"
# The corresponding fatality cases counts are placed in the "Dead" column
Deadcolumn <- Dead  %>%
  pivot_longer(cols = matches("^\\d{4}$"),   # new column with the Year
               names_to = "Year", values_to = "Dead",  # new column with the case counts
               names_transform = list(Year = as.integer))  # <-- ensures Year is integer



#Dead/Aggregate to Year x Age-group x Sex
DeadT <- Deadcolumn %>%
  group_by(Year, Sex, `Age-group`) %>%
  summarise(Dead = n()) %>%
  mutate(Year = as.numeric(Year))


# Merge both databases Cases and Dead.
DeadRR <- left_join(CasesT, DeadT, by = c("Sex", "Year", "Age-group")) 


#multivariate Logistic Regression 
modelFatality<- glm(cbind(Dead, Cases - Dead) ~ `Age-group` + Sex,
                      family = binomial(link = "log"),
                      data = DeadRR)

#Credible intervals 
modelFatality <- tidy(modelFatality, conf.int = TRUE) %>%
mutate(across(where(is.numeric), round, 2))

# Save results
write_xlsx(modelCfr_b, "~/path/to/your_file.xlsx")


