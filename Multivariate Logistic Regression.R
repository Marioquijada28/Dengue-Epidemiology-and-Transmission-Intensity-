

#Multivariate logistic regression models for dengue: incidence, hospitalisation, and fatality 
# Panama, 2000–2024

# Outputs:
#  - Adjusted IRR for reported incidence (Poisson with offset)
#  - Adjusted RR for hospitalisation among cases
#  - Adjusted RR for case fatality among cases



#Libraries 
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
Pop <- readRDS("path/to/PopDistrict2000-24.rds") #Population by Region and District
Dead  <- readRDS("path/to/Dead.rds") #Cases Fatality
Hospitalised <- readRDS("path/to/Hospitalised.rds") #Hospitalised Cases


#IRR for reported incidence
# Aggregate to Year x Age-group x Sex
CasesT <- Cases %>%
  group_by(Age-group, Year, Sex) %>%
  dplyr::summarize(Cases = sum(Cases, na.rm = TRUE))

#Merge Cases and Population 
Incage <- left_join(CasesT, Pop, by = c("Year", "Age", "Sex")) 

#multivariate Logistic Regression 
IncidenceModel <- glm(Cases ~ Age-group + Sex + offset(log(Popt)),
                      family = poisson (link = "log"), #log incidence rate ratios (IRR)
                      data =Incage)

#Credible intervals 
IncModel <- tidy(CasesModel, exponentiate = TRUE, conf.int = TRUE) 
mutate(across(where(is.numeric), round, 2))

# Save results
write_xlsx(CasesModel, "~/path/to/your_file.xlsx")






#HRR Hospitalisation among cases
# Hospitalised/Aggregate to Year x Age-group x Sex
Hosp <- Hospitalised%>%
  group_by(Year, Sex, Condition, Age-group) %>%
  summarise(Hosp = n(), .groups = "drop")

#Cases/Aggregate to Year x Age-group x Sex
CasesT <- Cases %>%
  group_by(Year, Age-group, Sex) %>%
  summarise(Cases = n(), .groups = "drop")

#Merge Hospitalised and Cases
HospitalizedSEX <- left_join(CasesT, Hosp, by = c("Sex", "Year", "Age-group")) 


#multivariate Logistic Regression 
model_Hosp <- glm(cbind(Hosp, Cases - Hosp) ~ Sex + Age-group ,
                  family = binomial(link = "log"),
                  data = rateHS)

#Credible intervals 
model_Hosp <- tidy(model_Hosp, exponentiate = TRUE, conf.int = TRUE)
mutate(across(where(is.numeric), round, 2))

# Save results
write_xlsx(model_Hosp, "~/path/to/your_file.xlsx")






##FRR Cases Fatality among cases
#Dead/Aggregate to Year x Age-group x Sex
DeadT <- Dead %>%
  group_by(Year, SEX, Age-group) %>%
  summarise(Dead = n()) %>%
  mutate(Year = as.numeric(Year))

# Cases/Aggregate to Year x Age-group x Sex
CasesT <- Cases %>%
  group_by(Year, Age-group, Sex) %>%
  summarise(Cases = n(), .groups = "drop")

# Merge both databases Cases and Dead.
DeadRR <- left_join(CasesT, DeadT, by = c("Sex", "Year", "Age-group")) 


#multivariate Logistic Regression 
modelFatality<- glm(cbind(Dead, Cases - Dead) ~ Age-group + Sex,
                      family = binomial(link = "logit"),
                      data = DeadRR)

#Credible intervals 
modelFatality<- tidy(modelFatality, conf.int = TRUE)
mutate(across(where(is.numeric), round, 2))

# Save results
write_xlsx(modelCfr_b, "~/path/to/your_file.xlsx")


