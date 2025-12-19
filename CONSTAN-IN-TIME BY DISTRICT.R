#----- Code to simulate & fit dengue constant-in-time FOI (District) catalytic model
# Produces the analyses and code used for the paper (Dengue Epidemiology and Transmission Intensity 
# across Panama during 2000-2024).

# Cite as:                                                                                
# Quijada M et al, Dengue Epidemiology and Transmission Intensity across Panama           
# during 2000-2024                                                                        
# Pre-print available at: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5461361%20  

#Data format expected (wide): Region, District, Age-group, Sex, 2000,2001,...,2024
#Model: district-level constant FOI (lambda), reporting prob rho and gamma region-level.

# Before running: edit USER CONFIG (file paths, region_to_run, cmdstan dir, stan model path)

#Libraries
library(ggplot2)
library(tidyr)
library("rstan")
library(cmdstanr)
library(readxl)
library(dplyr)
library(cowplot)
library(loo)
library(readr)       
library(purrr)

#Data-Read the .rds object
temp <- readRDS("path/to/DengueCases.rds") #Dengue Cases by Region and District
pop_mat <- readRDS("path/to/PopDistrict2000_24.rds") #Population by Region and District

#Cases
# Convert wide Year columns into long format
# Each Year becomes a category under "Year"
# The corresponding case counts are placed in the "Cases" column
Casescolumn <- Cases %>%
  pivot_longer(cols = matches("^\\d{4}$"),   # new column with the Year
               names_to = "Year", values_to = "Cases",  # new column with the case counts
               names_transform = list(Year = as.integer))  # <-- ensures Year is integer

# Aggregate to Year x Age-group x Region x District
Casescolumn <- Casescolumn %>%
  group_by(`Age-group`, Year, Region, District ) %>%
  summarise(Cases = sum(Cases),.groups = 'drop')

# Average cases across years for each Region × District x Age-group
#    (Mean over all years after the aggregation above)
CasesT <- Casescolumn  %>%
  group_by(`Age-group`, Region,  District) %>%
  dplyr::summarize(Cases = mean(Cases, na.rm = TRUE))

# Reshape to wide format:
#Each Age-group becomes its own column
temp <- CasesT %>%
  pivot_wider(names_from = `Age-group`,
              values_from = Cases)

# Remove NOT region and District categories
temp <- temp %>%
  filter(!Region %in% c("DESCONOCI", "IMPORTADO", "EXTRANJERO", "SIN DEFINIR"))
temp[is.na(temp)] <- 0 #Replace any remaining NA value

temp <- temp %>%
  filter(!District %in% c("Extranjero", "Desconocido", "Importado", "Missing", "N/A", "No Aplica"))



#Population
#    Reshape population data from wide (age columns) to long
#    Each age group becomes a row instead of a column
Pop <- Pop %>%
  pivot_longer(cols = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "over80"),
               names_to = "Age-group", values_to = "Pop")

#Aggregate population by Year × Age-group × Region x District
Pop24 <- Pop %>%
  group_by(`Age-group`, Year, Region,  District) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup()

#Compute mean population across years for each District x Region × Age-group
Popmean <- Pop24 %>%
  group_by(`Age-group`, Region, District) %>%
  summarise(Pop = as.integer(mean(Pop)),.groups = 'drop')

# Convert long population table into wide format
pop_mat <- Popmean %>%
  pivot_wider(names_from = `Age-group`,
              values_from = Pop) %>%
  ungroup


##DENGUE INTENSITY
#Model Setting
age <- 0:108
amin <- c(1,seq(11,81,10)) 
amax <- c(seq(10,80,10), 109) 
Location <- unique(temp$District)
Region <- unique(temp$Region)

  #Prepare Stan data list
  data <- list( nA= 9, # Age structure (9 groups)
                nL = length(Location), 
                nR = length(Region),
                max_age = 109,
                cases = as.matrix(temp[, -c(1:4)]), # int matrix (district x age groups)
                pop = as.matrix(pop_mat[, -c(1:4)]),  # numeric matrix (district x age groups)
                age=age,
                ageLims=rbind(amin,amax), # 2 x nA integer matrix of age bounds
                region = as.integer(factor(temp$Region))) #Region
  
  #Compile and fit the model
  check_cmdstan_toolchain(fix=T)
  set_cmdstan_path("/Users/marioquijada/.cmdstan/cmdstan-2.33.1")
  setwd("~/StanCode")
  mod <- cmdstan_model("Constant-in-time by District.stan", pedantic=T)
  
  fit <- mod$sample(data=data, 
                    chains=4, 
                    parallel_chains=4, 
                    iter_sampling=10000, 
                    refresh=1000, 
                    iter_warmup=5000)
  #output
  stanfit <- rstan::read_stan_csv(fit$output_files())
  saveRDS(stanfit, "~/path/to/your_file.rds")


  #Check convergence 
  trace <- traceplot(stanfit, pars=c('rho','gamma','lam_H'))
    ggsave("~/path/to/your_file.png",
    plot = trace,
           width = 30,
           height = 20,
           units = "in")


  #check convergence and quantiles 
  chains <- rstan::extract(stanfit)
  rho <- apply(chains$rho, 2, function(x) quantile(x, c(0.5, 0.025, 0.975)))
  gamma <- apply(chains$gamma, 2, function(x) quantile(x, c(0.5, 0.025, 0.975)))
  lam_H <- apply(chains$lam_H, 2, function(x) quantile(x, c(0.5, 0.025, 0.975)))  
  kids <- quantile(chains$kids, c(0.5, 0.025, 0.975))
  
  
  # Export parameter
  gamma <- t(gamma)
  lam_H <- t(lam_H)
  rho <- t(rho)

  #Arrange and label by Location
  rownames(gamma) <- Region
  rownames(lam_H) <- Location
  rownames(rho) <- Region

  # Convert to data frame
  gamma <- as.data.frame(gamma)
  lam_H <- as.data.frame(lam_H)
  rho <- as.data.frame(rho)
  
  #Export parameter summaries
  write.csv(rho, "~/path/to/your_file.csv")
  write.csv(gamma, "~/path/to/your_file.csv")
  write.csv(lam_H, "~/path/to/your_file.csv")
  write.csv(kids, "~/path/to/your_file.csv")


  
  # model fit
  #age-group 
  ageG <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','over80')

  fit <- data.frame(
    location = rep(Location, each = length(ageG)),
    age = rep(ageG, times = length(Location)),
    cases = as.vector(t(data$cases))
  )

  fit[, c('pred', 'ciL', 'ciU')] <- NA
  
  for (l in 1:data$nL) 
    for (a in 1:data$nA) {
      fit[fit$location == Location[l] & fit$age == ageG[a], 4:6] <- quantile(chains$Ecases[, l, a], c(0.5, 0.025, 0.975), na.rm = TRUE)
    }
  
  # Save as csv
  write.csv(fit, "~/path/to/your_file.csv")
  
  
  #Plot observed (points) vs predicted (line/ribbon) by District
  fit_plot <- ggplot(fit, aes(x = age, y = cases)) +
    geom_point() +
    geom_line(aes(y = pred, group = location), color = 'red') +
    geom_ribbon(aes(ymin = ciL, ymax = ciU, group = location), fill = 'red', alpha = 0.2) +
    ylab('Cases') +
    xlab('Age Group') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~ location, scales = "free_y")
  
  print(fit_plot)

# Save the plot
ggsave(
  "~/path/to/your_file.png",
  plot = fit_plot,
  width = 20,
  height = 12,
  units = "in"
)
