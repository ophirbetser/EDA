#------------------------------------------------------------------------------#
# script info and options ----
#### File Name:   EDA_ArgentinaCPI         
#### Date:        2022-08-14                
#### Author:      Ophir Betser              
#### Author Mail: ophir.betser@gmail.com    

#------------------------------------------------------------------------------#  
# env setup ----
## load functions
source('eda_functions.R')
## load packages
pacman::p_load(
  data.table,
  tidyverse,
  openxlsx,
  lubridate,
  skimr,
  DT,
  gt,
  ggpubr,
  shiny,
  dynlm,
  AER,
  visdat)
### set env
options(scipen=999) # Disables scientific notation          
options(digits=6)   # Limits the number of digits printed       
#------------------------------------------------------------------------------#  

# load data ----
data("ArgentinaCPI")
argentina <- ArgentinaCPI %>% data.table()
### read info about the data set
##### Time series of consumer price index (CPI) in Argentina 
##### (index with 1969(4) = 1).
##### Format
##### A quarterly univariate time series from 1970(1) to 1989(4).
?ArgentinaCPI


### first look
argentina %>% head(3)
skimr::skim(argentina)

#------------------------------------------------------------------------------#  
### set data up
year <- rep(seq(1970, 1989), each = 4)
quarter <- rep(seq(1, 4), time = 20)

argentina <- 
  data.table(
    year = year,
    quarter = quarter,
    CPI = argentina$.
  )

argentina$date <- 
  paste0(argentina$year, "-", argentina$quarter) %>% 
  as.yearqtr()

#------------------------------------------------------------------------------#  
# plot data ----
argentina %>% 
  ggplot() +
  aes(
    x = date,
    y = CPI
  ) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  labs(title = "CPI by date", subtitle = "")

argentina %>% 
  ggplot() +
  aes(
    x = date,
    y = log(CPI)
  ) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  labs(title = "CPI by date", subtitle = "")

#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  

# clean env ---- 
dev.off()
cat("\014")
rm(list = ls()) 

#------------------------------------------------------------------------------#  
data("ArgentinaCPI")
plot(ArgentinaCPI)
plot(log(ArgentinaCPI))

library("dynlm")
## estimation sample 1970.3-1988.4 means
acpi <- window(ArgentinaCPI, start = c(1970,1), end = c(1988,4)) 
## eq. (3.90), p.54

?dynlm
acpi_ols <- dynlm(d(log(acpi)) ~ L(d(log(acpi))))
summary(acpi_ols)

## alternatively
ar(diff(log(acpi)), order.max = 1, method = "ols") 





