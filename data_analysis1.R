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

table(recoded_data$CRT_1)
table(recoded_data$CRT_2)
table(recoded_data$CRT_3)
table(recoded_data$CRT_4)
table(recoded_data$CRT_5)
table(recoded_data$CRT_6)
table(recoded_data$CRT_7)



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
model1 <- '
VOI ~ CRT + a*CMQ + b*BRS
CMQ ~ c*CRT 
BRS ~ d*CRT
CMQ ~~ BRS
ind1 := a*c
ind2 := b*d
'

fit1 <- sem(model1, data = df)
summary(fit1, rsq = TRUE, standardized = TRUE)

#alternative model 
model2 <- '
VOI ~ DMQ + a*CMQ + b*BRS
CMQ ~ c*DMQ 
BRS ~ d*DMQ
CMQ ~~ BRS
ind1 := a*c
ind2 := b*d
'

fit2 <- sem(model2, data = df)
summary(fit2, rsq = TRUE, standardized = TRUE)




parameterestimates(fit)
lavCor(fit)

library(SemPlot)


#power analysis (no need to do)
#cronbach alpha on individual items 
#histograms (inspect)
#two paragraphs for each of the results 
#summarizing 


