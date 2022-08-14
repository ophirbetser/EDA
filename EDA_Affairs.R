#------------------------------------------------------------------------------#
# script info and options ----
#### File Name:   EDA_Affaris         
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
            AER,
            visdat)
### set env
options(scipen=999) # Disables scientific notation          
options(digits=6)   # Limits the number of digits printed       
#------------------------------------------------------------------------------#  

# load data ----
data("Affairs")
affairs <- Affairs %>% data.table()
### read info about the data set
?Affairs
# 1. this is a survey data set
# 2. 601 rows, 9 columns
# 3. some column have number encoding: affairs, age, yearsmarried, religiousness, education, rating

### first look
affairs %>% head(3)
skimr::skim(affairs)

# 1. no missing data
# 2. mostly numeric

#------------------------------------------------------------------------------#  
# one variable analysis ----
#### affairs
####### numeric.
####### How often engaged in extramarital sexual intercourse during the past year?
####### 0 = none,
####### 1 = once,
####### 2 = twice,
####### 3 = 3 times,
####### 7 = 4–10 times
####### 12 = monthly
####### 12 = weekly, 
####### 12 = daily

affairs[, .N, factor(affairs)] %>% one_var_barplot(x="factor", y="N")

affairs$affairs_decode <- 
  factor(
    fcase(
      affairs$affairs == 0, "none",
      affairs$affairs == 1, "once",
      affairs$affairs == 2, "twice",
      affairs$affairs == 3, "3 times",
      affairs$affairs == 7, "4–10 times",
      affairs$affairs == 12, "monthly"
    ),
    levels = c(
      "none",
      "once",
      "twice",
      "3 times",
      "4–10 times",
      "monthly"
    )
  )

affairs[, .N, affairs_decode] %>% one_var_barplot(x="affairs_decode", y="N") + 
  labs(title = "affairs", subtitle = "")

affairs$had_affairs <- 
  ifelse(
    affairs$affairs == 0, "no", "yes"
  )

affairs[, .N, had_affairs] %>% one_var_barplot(x="had_affairs", y="N") + 
  labs(title = "had_affairs", subtitle = "")

#### gender
table(affairs$gender)
##### female   male 
##### 315    286 

#### age
table(affairs$age)
##### numeric variable coding age in years: 
##### 17.5 = under 20, 
##### 22 = 20–24, 
##### 27 = 25–29, 
##### 32 = 30–34, 
##### 37 = 35–39, 
##### 42 = 40–44, 
##### 47 = 45–49, 
##### 52 = 50–54, 
##### 57 = 55 or over.
affairs$age_decode <- 
  factor(
    fcase(
      affairs$age == 17.5 ,"under 20",
      affairs$age == 22   ,"20–24", 
      affairs$age == 27   ,"25–29",
      affairs$age == 32   ,"30–34", 
      affairs$age == 37   ,"35–39",
      affairs$age == 42   ,"40–44", 
      affairs$age == 47   ,"45–49",
      affairs$age == 52   ,"50–54", 
      affairs$age == 57   ,"55 or over"
    ),
    levels = c(
      "under 20",
      "20–24", 
      "25–29",
      "30–34", 
      "35–39",
      "40–44", 
      "45–49",
      "50–54", 
      "55 or over")
  )
      
affairs[, .N, age_decode] %>% one_var_barplot(x="age_decode", y="N") + 
  labs(title = "age_decode", subtitle = "")

#### yearsmarried
table(affairs$yearsmarried)
##### numeric variable coding number of years married: 
##### 0.125 = 3 months or less, 
##### 0.417 = 4–6 months, 
##### 0.75 = 6 months–1 year, 
##### 1.5 = 1–2 years, 
##### 4 = 3–5 years, 
##### 7 = 6–8 years, 
##### 10 = 9–11 years, 
##### 15 = 12 or more years.
affairs$yearsmarried_decode <- 
  factor(
    fcase(
      affairs$yearsmarried == 0.125     ,"3 months or less", 
      affairs$yearsmarried == 0.417     ,"4–6 months", 
      affairs$yearsmarried == 0.75      ,"6 months–1 year", 
      affairs$yearsmarried == 1.5       ,"1–2 years", 
      affairs$yearsmarried == 4         ,"3–5 years", 
      affairs$yearsmarried == 7         ,"6–8 years", 
      affairs$yearsmarried == 10        ,"9–11 years", 
      affairs$yearsmarried == 15        ,"12 or more years"
    ),
    levels = c(
      "3 months or less", 
      "4–6 months", 
      "6 months–1 year", 
      "1–2 years", 
      "3–5 years", 
      "6–8 years", 
      "9–11 years", 
      "12 or more years")
    )

affairs[, .N, yearsmarried_decode] %>% one_var_barplot(x="yearsmarried_decode", y="N") + 
  labs(title = "yearsmarried_decode", subtitle = "")

#### children
table(affairs$children)
affairs[, .N, children] %>% one_var_barplot(x="children", y="N") + 
  labs(title = "children", subtitle = "")


#### religiousness
##### numeric variable coding religiousness: 
##### 1 = anti, 
##### 2 = not at all, 
##### 3 = slightly, 
##### 4 = somewhat, 
##### 5 = very.
affairs$religiousness_decode <- 
  factor(
    fcase(
      affairs$religiousness == 1, "anti",
      affairs$religiousness == 2, "not at all",
      affairs$religiousness == 3, "slightly",
      affairs$religiousness == 4, "somewhat",
      affairs$religiousness == 5, "very"
    ),
    levels = c(
      "anti",
      "not at all",
      "slightly",
      "somewhat",
      "very")
    )

affairs[, .N, religiousness_decode] %>% one_var_barplot(x="religiousness_decode", y="N") + 
  labs(title = "religiousness_decode", subtitle = "")

#### education
table(affairs$education)
##### numeric variable coding level of education: 
##### 9 = grade school, 
##### 12 = high school graduate, 
##### 14 = some college, 
##### 16 = college graduate, 
##### 17 = some graduate work, 
##### 18 = master's degree, 
##### 20 = Ph.D., M.D., or other advanced degree.
affairs$education_decode <- 
  factor(
    fcase(
      affairs$education == 9   ,"grade school",
      affairs$education == 12  ,"high school",
      affairs$education == 14  ,"some college",
      affairs$education == 16  ,"college graduate",
      affairs$education == 17  ,"graduate work",
      affairs$education == 18  ,"master's degree",
      affairs$education == 20  ,"Ph.D"
    ),
    levels = c(
      "grade school",
      "high school",
      "some college",
      "college graduate",
      "graduate work",
      "master's degree",
      "Ph.D")
    )
affairs[, .N, education_decode] %>% one_var_barplot(x="education_decode", y="N") + 
  labs(title = "education_decode", subtitle = "")


#### occupation
##### numeric variable coding occupation according 
##### to Hollingshead classification (reverse numbering).
affairs$occupation %>% table()
affairs[, .N, occupation] %>% one_var_barplot(x="occupation", y="N") + 
  labs(title = "occupation", subtitle = "")

#### rating
##### numeric variable coding self rating of marriage: 
##### 1 = very unhappy, 
##### 2 = somewhat unhappy, 
##### 3 = average, 
##### 4 = happier than average, 
##### 5 = very happy.
affairs$rating_decode <- 
  factor(
    fcase(
      affairs$rating == 1, "very unhappy",
      affairs$rating == 2, "somewhat unhappy",
      affairs$rating == 3, "average",
      affairs$rating == 4, "happier than average",
      affairs$rating == 5, "very happy"
    ),
    levels = c(
      "very unhappy",
      "somewhat unhappy",
      "average",
      "happier than average",
      "very happy")
    )

affairs[, .N, rating_decode] %>% one_var_barplot(x="rating_decode", y="N") + 
  labs(title = "rating_decode", subtitle = "")


#------------------------------------------------------------------------------#  
names(affairs)
g1 <- affairs[, .N, affairs_decode] %>% one_var_barplot(x="affairs_decode", y="N") + 
  labs(title = "affairs_decode", subtitle = "")

g2 <- affairs[, .N, gender] %>% one_var_barplot(x="gender", y="N") + 
  labs(title = "gender", subtitle = "")

g3 <- affairs[, .N, age_decode] %>% one_var_barplot(x="age_decode", y="N") + 
  labs(title = "age_decode", subtitle = "")

g4 <- affairs[, .N, yearsmarried_decode] %>% one_var_barplot(x="yearsmarried_decode", y="N") + 
  labs(title = "yearsmarried_decode", subtitle = "")

g5 <- affairs[, .N, children] %>% one_var_barplot(x="children", y="N") + 
  labs(title = "children", subtitle = "")

g6 <- affairs[, .N, religiousness_decode] %>% one_var_barplot(x="religiousness_decode", y="N") + 
  labs(title = "religiousness_decode", subtitle = "")

g7 <- affairs[, .N, education_decode] %>% one_var_barplot(x="education_decode", y="N") + 
  labs(title = "education_decode", subtitle = "")

g8 <- affairs[, .N, occupation] %>% one_var_barplot(x="occupation", y="N") + 
  labs(title = "occupation", subtitle = "")

g9 <- affairs[, .N, rating_decode] %>% one_var_barplot(x="rating_decode", y="N") + 
  labs(title = "rating_decode", subtitle = "")

ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 3, nrow = 3)

#------------------------------------------------------------------------------#  
# multivariable analysis ----
names(affairs)
g1 <- affairs[, .N, .(had_affairs, gender)] %>% 
  two_var_barplot("gender", "N", "had_affairs") +
  labs(title = "gender", subtitle = "")

g2 <- affairs[, .N, .(had_affairs, age_decode)] %>% 
  two_var_barplot("age_decode", "N", "had_affairs") +
  labs(title = "age_decode", subtitle = "")

g3 <- affairs[, .N, .(had_affairs, yearsmarried_decode)] %>% 
  two_var_barplot("yearsmarried_decode", "N", "had_affairs") +
  labs(title = "yearsmarried_decode", subtitle = "")

g4 <- affairs[, .N, .(had_affairs, children)] %>% 
  two_var_barplot("children", "N", "had_affairs") +
  labs(title = "children", subtitle = "")

ggarrange(g1, g2, g3, g4, ncol = 2, nrow = 2)

g5 <- affairs[, .N, .(had_affairs, religiousness_decode)] %>% 
  two_var_barplot("religiousness_decode", "N", "had_affairs") +
  labs(title = "religiousness_decode", subtitle = "")

g6 <- affairs[, .N, .(had_affairs, education_decode)] %>% 
  two_var_barplot("education_decode", "N", "had_affairs") +
  labs(title = "education_decode", subtitle = "")

g7 <- affairs[, .N, .(had_affairs, occupation)] %>% 
  two_var_barplot("occupation", "N", "had_affairs") +
  labs(title = "occupation", subtitle = "")

g8 <- affairs[, .N, .(had_affairs, rating_decode)] %>% 
  two_var_barplot("rating_decode", "N", "had_affairs") +
  labs(title = "rating_decode", subtitle = "")

ggarrange(g5, g6, g7, g8, ncol = 2, nrow = 2)

### had affairs rate ----
g1 <- had_affairs_rate("gender") %>% 
  one_var_barplot(x="gender", y="had_affairs_rate", label = F) + 
  labs(title = "gender", subtitle = "")

g2 <- had_affairs_rate("age_decode") %>% 
  one_var_barplot(x="age_decode", y="had_affairs_rate", label = F) + 
  labs(title = "age_decode", subtitle = "")

g3 <- had_affairs_rate("yearsmarried_decode") %>% 
  one_var_barplot(x="yearsmarried_decode", y="had_affairs_rate", label = F) + 
  labs(title = "yearsmarried_decode", subtitle = "")

g4 <- had_affairs_rate("children") %>% 
  one_var_barplot(x="children", y="had_affairs_rate", label = F) + 
  labs(title = "children", subtitle = "")

g5 <- had_affairs_rate("religiousness_decode") %>% 
  one_var_barplot(x="religiousness_decode", y="had_affairs_rate", label = F) + 
  labs(title = "religiousness_decode", subtitle = "")

g6 <- had_affairs_rate("education_decode") %>% 
  one_var_barplot(x="education_decode", y="had_affairs_rate", label = F) + 
  labs(title = "education_decode", subtitle = "")

g7 <- had_affairs_rate("occupation") %>% 
  one_var_barplot(x="occupation", y="had_affairs_rate", label = F) + 
  labs(title = "occupation", subtitle = "")

g8 <- had_affairs_rate("rating_decode") %>% 
  one_var_barplot(x="rating_decode", y="had_affairs_rate", label = F) + 
  labs(title = "rating_decode", subtitle = "")


ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 2, nrow = 4)

#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  
#------------------------------------------------------------------------------#  

# clean env ---- 
dev.off()
cat("\014")
rm(list = ls()) 


