#----- Code to simulate & fit dengue constant-in-time FOI (Region) catalytic model
# Produces the analyses and code used for the paper (Dengue Epidemiology and Transmission Intensity 
# across Panama during 2000-2024)

# Cite as:                                                                                
# Quijada M et al, Dengue Epidemiology and Transmission Intensity across Panama           
# during 2000-2024                                                                        
# Pre-print available at: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5461361%20  


#Data format expected (wide): Region, District, Age-group, Sex, 2000,2001,...,2024
#Model: region-level constant FOI (lambda), reporting prob rho and gamma.

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

#Read data 
temp <- readRDS("path/to/DengueCases.rds") #Dengue Cases by Region 
pop_mat <- readRDS("path/to/PopRegion2000_24.rds") #Population by Region 


##DENGUE INTENSITY
#Model Setting
age <- 0:108
amin <- c(1,seq(11,81,10))
amax <- c(seq(10,80,10), 109)
Location <- unique(temp$Region)

# Prepare Stan data list
  data <- list( nA= 9, # Age structure (9 groups)
               nL = length(Location), 
               max_age = 109,
               cases = as.matrix(temp[, -4]),   # int matrix (regions x age groups)
               pop = as.matrix(pop_mat[, -4]),   # numeric matrix (regions x age groups)
               age=age,
               ageLims=rbind(amin,amax)) # 2 x nA integer matrix of age bounds
  
  
  #Compile and fit the model
  check_cmdstan_toolchain(fix=T)
  set_cmdstan_path("/Users/marioquijada/.cmdstan/cmdstan-2.33.1")
  setwd("~StanCode")
  mod <- cmdstan_model("Constant-in-time by Region.stan", pedantic=T)
  
  fit <- mod$sample(data=data, 
                    chains=4, 
                    parallel_chains=4, 
                    iter_sampling=10000, 
                    refresh=1000, 
                    iter_warmup=5000)

  #output
  stanfit <- rstan::read_stan_csv(fit$output_files())
  saveRDS(stanfit, "/~path/to/your_file.rds")  #Save CmdStanMCMC object
  
  # Check convergence 
  trace <- traceplot(stanfit, pars = c('rho', 'gamma', 'lam_H'))
  ggsave(
    "~/path/to/your_file.png",
    plot = trace,
    width = 20,
    height = 16,
    units = "in"
  )
  
  #check convergence and quantiles for region-level parameters
  chains <- rstan::extract(stanfit)
  rho <- apply(chains$rho, 2, function(x) quantile(x, c(0.5, 0.025, 0.975)))
  gamma <- apply(chains$gamma, 2, function(x) quantile(x, c(0.5, 0.025, 0.975)))  
  lam <- apply(chains$lam_H, 2, function(x) quantile(x, c(0.5, 0.025, 0.975)))  
  kids <- quantile(chains$kids, c(0.5, 0.025, 0.975))

  # Export parameter 
  gamma <- t(gamma)
  lam <- t(lam)
  rho <- t(rho)
  
  #Arrange and label by region
  rownames(gamma) <- Location
  rownames(lam) <- Location
  rownames(rho) <- Location
  
  # Convert to data frame 
  gamma <- as.data.frame(gamma)
  lam <- as.data.frame(lam)
  rho <- as.data.frame(rho)
  kids <- as.data.frame(kids)

  # Export parameter summaries
  write.csv(rho, "~/path/to/your_file.csv")
  write.csv(gamma, "~/path/to/your_file.csv")
  write.csv(lam_H, "~/path/to/your_file.csv")
  write.csv(kids, "~/path/to/your_file.csv")


  
  #fit
  #age groups 
  ageG <- c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', 'over80')
  fit <- data.frame(
    location = rep(Location, each = length(ageG)),
    age = rep(ageG, times = length(Location)),
    cases = as.vector(t(data$cases))
  )
  
  fit[, c('pred', 'ciL', 'ciU')] <- NA
  
  for (l in 1:data$nL) for(a in 1:data$nA) {
    fit[fit$location== Location[l] & fit$age == ageG[a],4:6] <- quantile(chains$Ecases[, l, a], c(0.5, 0.025, 0.975), na.rm = TRUE) 
  }
  
  # Convert 'age' and regions' columns to factors for plotting
  fit$age <- factor(fit$age, levels = unique(fit$age))
  fit$location <- factor(fit$location, levels = Location)
  
  #Plot observed (points) vs predicted (line/ribbon) by region
  fit_plot <- ggplot(fit, aes(x = age, y = cases)) +
    geom_point() +
    geom_line(aes(y = pred, group = location), color = 'blue') +
    geom_ribbon(aes(ymin = ciL, ymax = ciU, group = location), fill = 'blue', alpha = 0.2) +
    ylab('Cases') +
    xlab('Age Group') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~ location, scales = "free_y")
  
  print(fit_plot)
  
  #SavePlot
  ggsave(
    "~/path/to/your_file.png",
    plot = fit_plot,
    width = 12,
    height = 8,
    units = "in"
  )
  
  
  
  


  
  
  
  
  
  
  
  
  
  
  
  
 
