library(haven)
library(tidyverse)

raw_data <- read_sav("data_deidentified.sav")
raw_data

glimpse(raw_data)

working_data <- raw_data %>% 
  rename(CRT_1 = CRT_Q1, CRT_2 = CRT_Q2)%>% 
  rename(check1 = BRS_11, check2 = DMQ_11)
  

recoded_data <- working_data %>% 
  mutate(across(starts_with("CRT_"), 
                ~ifelse(. == 1, 1, 0), 
         .names = "{.col}")) 
#Note that all right answers were the option 1 for CRT items
glimpse(recoded_data)



recoded_data <- recoded_data %>% 
  #keeping only the rows that passed attention check
  filter(check1 == 5 & check2 == 1)%>%
  #taking means
  mutate(CRT = rowMeans(across(starts_with("CRT_"))),
         CMQ = rowMeans(across(starts_with("CMQ"))), 
         BRS = rowMeans(across(starts_with("BRS_"))),
         VOI = rowMeans(across(starts_with("VOI"))),
         )
glimpse(recoded_data)
#attention check filtered out five participants
#our n = 5
recoded_data <- recoded_data %>% 
  mutate(rational = (DMQ_1+DMQ_2+ DMQ_3+ DMQ_4+ DMQ_5)/5, 
         intuitive = (DMQ_6+DMQ_7+ DMQ_8+ DMQ_9+ DMQ_10)/5)

glimpse(recoded_data)
cor(recoded_data$rational, recoded_data$intuitive) 
#r = -0.46 (as expected)
cor(recoded_data$rational, recoded_data$CRT )
# r = 0.12 (weak correlation, little concerning)
cor(recoded_data$intuitive, recoded_data$CRT )
#r = -0.15 (weak correlation, little concerning)

#reverse-coding the intuitive items to combine with rational
recoded_data <- recoded_data %>% 
  mutate(intuitive_reverse = 6 - intuitive)%>%
  mutate(DMQ = (rational + intuitive_reverse)/2)

glimpse(recoded_data)
cor(recoded_data$CRT, recoded_data$DMQ )
#cor 0.16, not bad

df <- recoded_data %>% 
  select(CRT, CMQ, BRS, VOI, DMQ)

head(df)

library(lavaan)
model <- '
VOI ~ CRT
VOI ~ CMQ + BRS 
VOI ~ CRT + CMQ + BRS
CMQ ~ CRT 
BRS ~ CRT
'
fit <- sem(model, data = df)
summary(fit, rsq = TRUE)
parameterestimates(fit)
lavCor(fit)
