#------------------------------------------------------------------------------#
# script info and options ----
#### File Name:   STAT_cross_correlation         
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
# https://www.r-bloggers.com/2021/08/how-to-calculate-cross-correlation-in-r/

Spend <- c(5, 3, 6, 5, 8, 9, 10, 17, 12, 11, 10, 9)
Income <- c(25, 29, 22, 34, 22, 28, 29, 31, 34, 45, 45, 40)
ccf(Spend, Income)

plot(Income, Spend)
#------------------------------------------------------------------------------#  

dt <- 
  data.table(
    spend = Spend,
    income = Income
  )

dt[, spend_sm4:= shift(spend,-4)]
dt[, spend_sm3:= shift(spend,-3)]
dt[, spend_sm2:= shift(spend,-2)]
dt[, spend_sm1 := shift(spend,-1)]
dt[, spend_s0 := shift(spend,0)]
dt[, spend_s1 := shift(spend,1)]
dt[, spend_s2 := shift(spend,2)]
dt[, spend_s3 := shift(spend,3)]
dt[, spend_s4 := shift(spend,4)]

visdat::vis_cor(dt)

ggplot(dt) +
  #geom_jitter(aes(x = income, y = spend)) +
  #geom_jitter(aes(x = income, y = spend_s1), color = "blue") +
  #geom_jitter(aes(x = income, y = spend_s2), color = "red") +
  geom_jitter(aes(x = income, y = spend_s3), color = "green") +
  #geom_jitter(aes(x = income, y = spend_s4), color = "orange") +
  theme_classic2()

#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  

# clean env ---- 
dev.off()
cat("\014")
rm(list = ls()) 
