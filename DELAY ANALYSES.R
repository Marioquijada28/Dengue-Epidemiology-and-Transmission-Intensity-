##DELAYS DISTRIBUTIO WITH 5 PARAMETERS.

library(writexl)
library(readxl)
library(tidyverse)
library(dplyr)
library(cowplot)
library(ggpubr)
library("fitdistrplus")
library(flexsurv)
library("gridGraphics")
library(boot)
library(lubridate)

theme_set(theme_classic())

# Add data base
DR <- readRDS("path/to/LineList_2000_24.rds") #Delay report


# Convert date strings to Date objects with specified format
DR$`DATE SAMPLE COLLECTION` <- as.Date(DR$`DATE SAMPLE COLLECTION`, format = "%Y-%m-%d")
DR$`DATE ONSET SYMPTOMS` <- as.Date(DR$`DATE ONSET SYMPTOMS`, format = "%Y-%m-%d")
DR$`DATE REPORTING` <- as.Date(DR$`DATE REPORTING`, format = "%Y-%m-%d")
DR$`DATE HOSPITALISATION` <- as.Date(DR$`DATE HOSPITALISATION`, format = "%Y-%m-%d")
DR$`DATE RECOVERY` <- as.Date(DR$`DATE RECOVERY`, format = "%Y-%m-%d")


# Subtract without filter
DR$OSSC <-  as.numeric(DR$`DATE SAMPLE COLLECTION` - DR$`DATE ONSET SYMPTOMS`)
DR$OSR <-  as.numeric(DR$`DATE REPORTING` - DR$`DATE ONSET SYMPTOMS`)
DR$OSDH <-  as.numeric(DR$`DATE HOSPITALISATION` - DR$`DATE ONSET SYMPTOMS`)
DR$OSDR <-  as.numeric(DR$`DATE RECOVERY` - DR$`DATE ONSET SYMPTOMS`)
DR$HDR <-  as.numeric(DR$`DATE RECOVERY` - DR$`DATE HOSPITALISATION`)

# Group
delay <- DR %>%
  dplyr::select(OSR, OSDH, OSSC, OSDR, HDR, Region, Sexo, EDAD)


# Overview of the fitdistrplus package
delay1 <- subset(delay, OSSC >= 0 & OSSC <= 20)
delay2 <- subset(delay, OSR >= 0 & OSR <= 20)
delay3 <- subset(delay, OSDH >= 0 & OSDH <= 20)
delay4 <- subset(delay, OSDR >= 0 & OSDR <= 20)
delay5 <- subset(delay, HDR >= 0 & HDR <= 20)






###COUNTRY ANALYSES
#1DATE ONSET SYMPTOMS - DATE SAMPLE COLLECTION
delay1$OSSC <- as.numeric(delay1$OSSC)
data1 <- delay1$OSSC[delay1$OSSC > 0]

descdist(delay1$OSSC, boot = 1000)

par(mfrow = c(1, 1))
fw1 <- fitdist(data1, "weibull")
fg1 <- fitdist(data1, "gamma")
fln1 <- fitdist(data1, "lnorm")
fita1 <- fitdist(data1, "llogis")
cau1 <- fitdist(data1, "cauchy")


#2DATE ONSET SYMPTOMS  - DATE REPORTING
delay2$OSR <- as.numeric(delay2$OSR)
data <- delay2$OSR[delay2$OSR > 0]

descdist(delay2$OSR, boot = 1000)

fw2 <- fitdist(data2, "weibull")
fg2 <- fitdist(data2, "gamma")
fln2 <- fitdist(data2, "lnorm")
fita2 <- fitdist(data2, "llogis")
cau2 <- fitdist(data2, "cauchy")


#3DATE ONSET SYMPTOMS - DATE HOSPITALISATION
delay3$OSDH <- as.numeric(delay3$OSDH)
data3 <- delay3$OSDH[delay3$OSDH > 0]

descdist(delay3$OSDH, boot = 1000)

fw3 <- fitdist(data3, "weibull")
fg3 <- fitdist(data3, "gamma")
fln3 <- fitdist(data3, "lnorm")
fita3 <- fitdist(data3, "llogis")
cau3 <- fitdist(data3, "cauchy")


#4 DATE ONSET SYMPTOMS - DATE RECOVERY
delay5$OSDR <- as.numeric(delay5$OSDR)
data3 <- delay5$OSDR[delay5$OSDR > 0]

descdist(delay5$OSDR)

par(mfrow = c(1, 1))
fw5 <- fitdist(data5, "weibull")
fg5 <- fitdist(data5, "gamma")
fln5 <- fitdist(data5, "lnorm")
fita5 <- fitdist(data5, "llogis")
cau5 <- fitdist(data5, "cauchy")


#5 HOSPITALISATION - DATE RECOVERY
delay5$HDR <- as.numeric(delay5$HDR)
data5 <- delay5$HDR[delay5$HDR > 0]

descdist(delay5$HDR)

fw5 <- fitdist(data5, "weibull")
fg5 <- fitdist(data5, "gamma")
fln5 <- fitdist(data5, "lnorm")
fita5 <- fitdist(data5, "llogis")
cau5 <- fitdist(data5, "cauchy")

# Save the results to a CSV file
fileDNR <- "~/path/to/NationalDelayResults.xlsx"





#AIC.
#Final Results 
# DATE ONSET SYMPTOMS - DATE SAMPLE COLLECTION
results1 <- gofstat(list(fw2, fg2, fln2, fita2, cau2))

# DATE ONSET SYMPTOMS - DATE REPORTING
results2 <- gofstat(list(fw, fg, fln, fita, cau))

# DATE ONSET SYMPTOMS - DATE HOSPITALISATION
results3 <- gofstat(list(fw1, fg1, fln1, fita1, cau1))

# DATE ONSET SYMPTOMS - DATE RECOVERY
results4 <- gofstat(list(fw3, fg3, fln3, fita3, cau3))

# Hospitalisation - DATE RECOVERY
results5 <- gofstat(list(fw4, fg4, fln4, fita4, cau4))



# Create a data frame for each set of results
df1 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE SAMPLE COLLECTION",
  Kolmogorov_Smirnov = results1$ks,
  Cramer_von_Mises = results1$cvm,
  Anderson_Darling = results1$ad,
  AIC = results1$aic,
  BIC = results1$bic
)

df2 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE REPORTING",
  Kolmogorov_Smirnov = results2$ks,
  Cramer_von_Mises = results2$cvm,
  Anderson_Darling = results2$ad,
  AIC = results2$aic,
  BIC = results2$bic
)

df3 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE HOSPITALISATION",
  Kolmogorov_Smirnov = results3$ks,
  Cramer_von_Mises = results3$cvm,
  Anderson_Darling = results3$ad,
  AIC = results3$aic,
  BIC = results3$bic
)


df4 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE RECOVERY",
  Kolmogorov_Smirnov = results4$ks,
  Cramer_von_Mises = results4$cvm,
  Anderson_Darling = results4$ad,
  AIC = results4$aic,
  BIC = results4$bic
)

df5 <- data.frame(
  Category = "Hospitalisation - DATE RECOVERY",
  Kolmogorov_Smirnov = results5$ks,
  Cramer_von_Mises = results5$cvm,
  Anderson_Darling = results5$ad,
  AIC = results5$aic,
  BIC = results5$bic
)

# Combine all data frames into one
combined_C <- rbind(df1, df2, df3, df4, df5)

fileC <- ("~/path/to/CountryAIC.xlsx")





##Delay by Sex
#1  "DATE ONSET SYMPTOMS - DATE SAMPLE COLLECTION")
#Male
M1 <- delay1 %>%
  filter(SEXO == "MALE" & OSSC > 0) %>%
  pull(OSSC) %>%
  as.numeric()

descdist(M1)

wfM1 <- fitdist(M1, "weibull")
gfM1 <- fitdist(M1, "gamma")
lnfM1 <- fitdist(M1, "lnorm")
lfM1 <- fitdist(M1, "llogis")
caufM1 <- fitdist(M1, "cauchy")


#female
F1 <- delay1 %>%
  filter(SEXO == "FEMALE" & OSSC > 0) %>%
  pull(OSSC) %>%
  as.numeric()

descdist(F1)

wfF1 <- fitdist(F1, "weibull")
gfF1 <- fitdist(F1, "gamma")
lnfF1 <- fitdist(F1, "lnorm")
lfF1 <- fitdist(F1, "llogis")
caufF1 <- fitdist(F1, "cauchy")



#2#DATE ONSET SYMPTOMS  - DATE REPORTING
#Male
M2 <- delay2 %>%
  filter(SEXO == "MALE" & OSR > 0) %>%
  pull(OSR) %>%
  as.numeric()
descdist(M2)


#female
F2 <- delay2 %>%
  filter(SEXO == "FEMALE" & OSR > 0) %>%
  pull(OSR) %>%
  as.numeric()

descdist(F2)

wfF2 <- fitdist(F2, "weibull")
gfF2 <- fitdist(F2, "gamma")
lnfF2 <- fitdist(F2, "lnorm")
lfF2 <- fitdist(F2, "llogis")
caufF2 <- fitdist(F2, "cauchy")



#3 "DATE ONSET SYMPTOMS - DATE HOSPITALISATION")
#Male
M3 <- delay3 %>%
  filter(SEXO == "MALE" & OSDH > 0) %>%
  pull(OSDH) %>%
  as.numeric()

descdist(M3)

wfF3 <- fitdist(F3, "weibull")
gfF3 <- fitdist(F3, "gamma")
lnfF3 <- fitdist(F3, "lnorm")
lfF3 <- fitdist(F3, "llogis")
caufF3 <- fitdist(F3, "cauchy")


#female
F3 <- delay3 %>%
  filter(SEXO == "FEMALE" & OSDH > 0) %>%
  pull(OSDH) %>%
  as.numeric()

descdist(F3)

par(mfrow = c(1, 1))
wfF3 <- fitdist(F3, "weibull")
gfF3 <- fitdist(F3, "gamma")
lnfF3 <- fitdist(F3, "lnorm")
lfF3 <- fitdist(F3, "llogis")
caufF3 <- fitdist(F3, "cauchy")



#4 "DATE ONSET SYMPTOMS - DATE RECOVERY")
M4 <- delay5 %>%
  filter(SEXO == "MALE" & OSDR > 0) %>%
  pull(OSDR) %>%
  as.numeric()

descdist(M4)

wfM4 <- fitdist(M4, "weibull")
gfM4 <- fitdist(M4, "gamma")
lnfM4 <- fitdist(M4, "lnorm")
lfM4 <- fitdist(M4, "llogis")
caufM4 <- fitdist(M4, "cauchy")

#female
F4 <- delay5 %>%
  filter(SEXO == "FEMALE" & OSDR > 0) %>%
  pull(OSDR) %>%
  as.numeric()

descdist(F4)

par(mfrow = c(1, 1))
wfF4 <- fitdist(F4, "weibull")
gfF4 <- fitdist(F4, "gamma")
lnfF4 <- fitdist(F4, "lnorm")
lfF4 <- fitdist(F4, "llogis")
caufF4 <- fitdist(F4, "cauchy")


#5 "Hospitalisation - DATE RECOVERY")
M5 <- delay5 %>%
  filter(SEXO == "MALE" & HDR > 0) %>%
  pull(HDR) %>%
  as.numeric()

descdist(M5)

wfM5 <- fitdist(M5, "weibull")
gfM5 <- fitdist(M5, "gamma")
lnfM5 <- fitdist(M5, "lnorm")
lfM5 <- fitdist(M5, "llogis")
caufM5 <- fitdist(M5, "cauchy")


#female
F5 <- delay5 %>%
  filter(SEXO == "FEMALE" & HDR > 0) %>%
  pull(HDR) %>%
  as.numeric()

descdist(F5)

wfF5 <- fitdist(F5, "weibull")
gfF5 <- fitdist(F5, "gamma")
lnfF5 <- fitdist(F5, "lnorm")
lfF5 <- fitdist(F5, "llogis")
caufF5 <- fitdist(F5, "cauchy")



#Table INFORMATION about the delay distribution by sex.
#FEMALE TABLE
# DATE ONSET SYMPTOMS - DATE REPORTING Female
resultsF1 <- gofstat(list(wfF1, gfF1, lnfF1, lfF1, caufF1))

# DATE ONSET SYMPTOMS - DATE HOSPITALISATION Female
resultsF2 <- gofstat(list(wfF2, gfF2, lnfF2, lfF2, caufF2))

# DATE ONSET SYMPTOMS - DATE SAMPLE COLLECTION Female
resultsF3 <- gofstat(list(wfF3, gfF3, lnfF3, lfF3, caufF3))

# DATE ONSET SYMPTOMS - DATE RECOVERY Female
resultsF4 <- gofstat(list(wfF4, gfF4, lnfF4, lfF4, caufF4))

#DATE HOSPITALISATION  - DATE RECOVERY Female
resultsF5 <- gofstat(list(wfF5, gfF5, lnfF5, lfF5, caufF5))


# Create a data frame for each set of results
dfF1 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE SAMPLE COLLECTION Female",
  Kolmogorov_Smirnov = resultsF1$ks,
  Cramer_von_Mises = resultsF1$cvm,
  Anderson_Darling = resultsF1$ad,
  AIC = resultsF1$aic,
  BIC = resultsF1$bic
)

dfF2 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE REPORTING Female",
  Kolmogorov_Smirnov = resultsF2$ks,
  Cramer_von_Mises = resultsF2$cvm,
  Anderson_Darling = resultsF2$ad,
  AIC = resultsF2$aic,
  BIC = resultsF2$bic
)


dfF3 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE HOSPITALISATION Female",
  Kolmogorov_Smirnov = resultsF3$ks,
  Cramer_von_Mises = resultsF3$cvm,
  Anderson_Darling = resultsF3$ad,
  AIC = resultsF3$aic,
  BIC = resultsF3$bic
)


dfF4 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE RECOVERY Female",
  Kolmogorov_Smirnov = resultsF4$ks,
  Cramer_von_Mises = resultsF4$cvm,
  Anderson_Darling = resultsF4$ad,
  AIC = resultsF4$aic,
  BIC = resultsF4$bic
)

dfF5 <- data.frame(
  Category = "DATE HOSPITALISATION  - DATE RECOVERY Female",
  Kolmogorov_Smirnov = resultsF5$ks,
  Cramer_von_Mises = resultsF5$cvm,
  Anderson_Darling = resultsF5$ad,
  AIC = resultsF5$aic,
  BIC = resultsF5$bic
)

# Combine all data frames into one
fileaicF <- ("~/Path/to/CountryAICSexF.xlsx")
write_xlsx(combined_F, fileaicF)



#MALE TABLE
# DATE ONSET SYMPTOMS - DATE SAMPLE COLLECTION Male
resultsM1 <- gofstat(list(wfM1, gfM1, lnfM1, lfM1, caufM1))

# DATE ONSET SYMPTOMS - DATE REPORTING Male
resultsM2 <- gofstat(list(wfM2, gfM2, lnfM2, lfM2, caufM2))

# DATE ONSET SYMPTOMS - DATE HOSPITALISATION Male
resultsM3 <- gofstat(list(wfM3, gfM3, lnfM3, lfM3, caufM3))

# DATE ONSET SYMPTOMS - DATE RECOVERY Male
resultsM4 <- gofstat(list(wfM4, gfM4, lnfM4, lfM4, caufM4))

# Hospitalisation - DATE RECOVERY Male
resultsM5 <- gofstat(list(wfM5, gfM5, lnfM5, lfM5, caufM5))


# Create a data frame for each set of results
dfM1 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE SAMPLE COLLECTION Male",
  Kolmogorov_Smirnov = resultsM1$ks,
  Cramer_von_Mises = resultsM1$cvm,
  Anderson_Darling = resultsM1$ad,
  AIC = resultsM1$aic,
  BIC = resultsM1$bic
)

dfM2 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE REPORTING Male",
  Kolmogorov_Smirnov = resultsM2$ks,
  Cramer_von_Mises = resultsM2$cvm,
  Anderson_Darling = resultsM2$ad,
  AIC = resultsM2$aic,
  BIC = resultsM2$bic
)

dfM3 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE HOSPITALISATION Male",
  Kolmogorov_Smirnov = resultsM3$ks,
  Cramer_von_Mises = resultsM3$cvm,
  Anderson_Darling = resultsM3$ad,
  AIC = resultsM3$aic,
  BIC = resultsM3$bic
)


dfM4 <- data.frame(
  Category = "DATE ONSET SYMPTOMS - DATE RECOVERY Male",
  Kolmogorov_Smirnov = resultsM4$ks,
  Cramer_von_Mises = resultsM4$cvm,
  Anderson_Darling = resultsM4$ad,
  AIC = resultsM4$aic,
  BIC = resultsM4$bic
)

dfM5 <- data.frame(
  Category = "Hospitalisation - DATE RECOVERY Male",
  Kolmogorov_Smirnov = resultsM5$ks,
  Cramer_von_Mises = resultsM5$cvm,
  Anderson_Darling = resultsM5$ad,
  AIC = resultsM5$aic,
  BIC = resultsM5$bic
)

# Combine all data frames into one
combined_M <- rbind(dfM1, dfM2, dfM3, dfM4, dfM5)
fileAICM <- ("~/Path/to/CountryAICSexM.xlsx")
write_xlsx(combined_M, filAICM)







