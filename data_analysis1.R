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

df1 <- df1 %>% 
  filter(check1 == 5 & check2 == 1) 

#cronbach alpha
library(psych)
CRT_items <- df1 %>% select(starts_with("CRT_"))
glimpse(CRT_items)
psych::alpha(CRT_items)

VOI_items <- df1 %>% select(starts_with(("VOI")))
glimpse(VOI_items)
psych::alpha(VOI_items)

DMQ_items <- df1 %>% select(starts_with("DMQ"))
glimpse(DMQ_items)
psych::alpha(DMQ_items, check.keys = TRUE )

CMQ_items <- df1 %>% select(starts_with("CMQ"))
glimpse(CMQ_items)
psych::alpha(CMQ_items)

BRS_items <- df1 %>% select(starts_with("BRS_"))
glimpse(BRS_items)
psych::alpha(BRS_items)

df2 <- df1 %>% 
  #aggregating
  mutate(CRT = rowMeans(across(starts_with("CRT_"))),
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
summary(fit1, rsq = TRUE, standardized = TRUE, fit.measures = TRUE)

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
summary(fit2, rsq = TRUE, standardized = TRUE, fit.measures = TRUE)

semPaths(fit2, what = "est", layout =  "tree2", sizeMan = 10, sizeLat = 15, 
         residuals = FALSE, edge.label.cex = 0.8, color = "lightblue", 
         edge.color = "red")


parameterestimates(fit1)
fitMeasures(fit1)
parameterestimates(fit2)
lavCor(fit)

#bootstrapping 
fit_boot <- sem(model1, data = df, se = "bootstrap", bootstrap = 5000)
summary(fit_boot, rsq = TRUE, standardized = TRUE, fit.measures = TRUE)

semPaths(fit_boot, what = "est", layout =  "tree2", sizeMan = 10, sizeLat = 15, 
         residuals = FALSE, edge.label.cex = 0.8, color = "lightblue", 
         edge.color = "red")

parameterEstimates(fit_boot)


#normality test

par(mfrow = c(2,3) )
hist(df$CRT)
hist(df$CMQ)
hist(df$BRS)
hist(df$VOI)
hist(df$DMS)

qqnorm(df$CRT)

shapiro.test(df$CRT)
shapiro.test(df$CMQ)
shapiro.test(df$VTM)
shapiro.test(df$BRS)
shapiro.test(df$DMS)

#the following code didn't work
'''
library(semPower)
semPower.postHoc(alpha = 0.05,
                 N = 194,
                 power = 0.80, 
                 effect = 0.3, 
                 nObserved = 29, 
                 nLatent = 4)

library(simsem)
power_result <- sim(model = model1, n = 200, nRep = 1000, generate = model, 
                    paramNames = list(b = 0.3))
summary(power_result)
'''
###


#trying some plots 
library(lavaanPlot)
lavaanPlot(model = fit1, coef = TRUE, stand = TRUE, covs = TRUE, stars = "regress")
lavaanPlot(model = fit2, coef = TRUE, stand = TRUE, covs = TRUE, stars = "regress")
lavaanPlot(model = fit_boot, coef = TRUE, stand = TRUE, covs = TRUE, stars = "regress")

