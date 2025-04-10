library(haven)
library(tidyverse)

raw_data <- read_sav("data_deidentified.sav")

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

#some frequency analysis 
#originally recruited: 200 participants, all from US 
#1 rejected 
recoded_data <- recoded_data %>% 
  mutate(sex = factor(sex, 
                      levels = c('1','2','3','4'), 
                      labels = c("Male", "Female", "Non_binary", "Not disclosed") 
                      ))%>% 
  mutate(race = factor(race, 
                       levels = c(1, 2, 3, 4, 6), 
                       labels = c("White", "Black", "Other", "Asian", "Other")
                       ))
  

df1 <- recoded_data
summary(df1$age_1)
sd(df1$age_1)

table(df1$sex)
table(df1$race)



df2 <- df1 %>% 
  #keeping only the rows that passed attention check
  filter(check1 == 5 & check2 == 1)%>%
  #aggregating
  mutate(CRT = rowSums(across(starts_with("CRT_"))),
         CMQ = rowMeans(across(starts_with("CMQ"))), 
         BRS = rowMeans(across(starts_with("BRS_"))),
         VOI = rowMeans(across(starts_with("VOI"))),
         )
glimpse(df2)
#attention check filtered out five participants
#our n = 194

df2 <- df2 %>% 
  mutate(rational = (DMQ_1+DMQ_2+ DMQ_3+ DMQ_4+ DMQ_5)/5, 
         intuitive = (DMQ_6+DMQ_7+ DMQ_8+ DMQ_9+ DMQ_10)/5)


glimpse(df2)

cor(df2$rational, df2$intuitive) 
#r = -0.46 (as expected)
cor(df2$rational, df2$CRT)
# r = 0.12 (weak correlation, little concerning)
cor(df2$intuitive, df2$CRT )
#r = -0.15 (weak correlation, little concerning)

#reverse-coding the intuitive items to combine with rational
df2 <- df2 %>% 
  mutate(intuitive_reverse = 6 - intuitive)%>%
  mutate(DMS = (rational + intuitive_reverse)/2)

glimpse(df2)
cor(df2$CRT, df2$DMS )
#cor 0.16, not bad

df <- df2 %>% 
  select(CRT, CMQ, BRS, VOI, DMS)

head(df)
ggplot(df, aes(x = CRT))+geom_density()

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

library(semPlot)
semPaths(fit1, what = "est", layout =  "tree2", sizeMan = 10, sizeLat = 15, 
         residuals = FALSE, edge.label.cex = 0.8, color = "lightblue", 
         edge.color = "red")

#alternative model 
model2 <- '
VOI ~ DMS + a*CMQ + b*BRS
CMQ ~ c*DMS 
BRS ~ d*DMS
CMQ ~~ BRS
ind1 := a*c
ind2 := b*d
'

fit2 <- sem(model2, data = df)
summary(fit2, rsq = TRUE, standardized = TRUE)

semPaths(fit2, what = "est", layout =  "tree2", sizeMan = 10, sizeLat = 15, 
         residuals = FALSE, edge.label.cex = 0.8, color = "lightblue", 
         edge.color = "red")


parameterestimates(fit1)
lavCor(fit)

#bootstrapping 
fit_boot <- sem(model1, data = df, se = "bootstrap", bootstrap = 5000)
summary(fit_boot, rsq = TRUE, standardized = TRUE)

semPaths(fit_boot, what = "est", layout =  "tree2", sizeMan = 10, sizeLat = 15, 
         residuals = FALSE, edge.label.cex = 0.8, color = "lightblue", 
         edge.color = "red")

#bootstrap failed
lavInspect(fit1)

#power analysis (no need to do)
#cronbach alpha on individual items 
#histograms (inspect)
#two paragraphs for each of the results 
#summarizing 




df3 <- read_csv("raw_dataset.csv")
glimpse(df3)
table(df3$CRT_Q1[6:216 ])
table(df3$CRT_3[6:216])
table(recoded_data$CRT_3)
