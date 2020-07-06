#####The way of making a choice: maximizing and satisficing as predictors of happiness... or depression?####

##load data

library(readxl)
data <- read_excel("raw_data.xlsx")

#load packages

library(tidyverse)
library(psych)
library(GPArotation)
library(mctest)
library(rockchalk)
library(apaTables)
library(lm.beta)
library(MissMech)
library(mice)
library(miceadds)
library(mitools)

####data preparation####

data <- data %>% 
  mutate_at("Gender", funs(recode(., `0`="male", `1`="female"))) %>% 
  mutate_at("Status", funs(recode(., `0`="single", `1`="in relationship", `2`="married", `3`="divorced"))) %>% 
  mutate_at("Education", funs(recode(., `0`="primary school", `1`="high school without graduation", `2`="high school with graduation", `3`="bachelor", `4`="master", `5`="doctoral"))) %>% 
  mutate_at("Type of data collection", funs(recode(., `0`="online", `1`="personal")))

####inspect categories
table(data$Gender)
table(data$Status)
table(data$Education)
table(data$`Type of data collection`)

####inspecting unuseal values in numeric
table(data$Age)
lengths(lapply(data[,6:12], unique)) #MTS_7 items answer on items should be from 1-5
lengths(lapply(data[,13:24], unique)) #MI_AS items answer on items should be from 1-5
lengths(lapply(data[,25:28], unique)) #DMTI_las items answer on items should be from 1-5
lengths(lapply(data[,29:44], unique)) #BFI_44 items answer on items should be from 1-5
lengths(lapply(data[,45:48], unique)) #SHS items answer on items should be from 1-7
lengths(lapply(data[,49:69], unique)) #BDI2 items answer on items should be from 0-3
lengths(lapply(data[,70:81], unique)) #RRQ items answer on items should be from 1-5

####missing age
sum(is.na(data$Age))/prod(dim(data$Age))

## Remove cases with age lower than 18 = it is exclusion criteron
data <- data[!data$Age < 18, ]

## Remove cases with NA in age = we can be sure if the participant is not under 18 
data <- data %>% drop_na(Age)

#####missing values
sum(is.na(data))/prod(dim(data))
colMeans(is.na(data))
md.pattern(data) 

##remove cases with more than 80% NA
data <- data[which(rowMeans(!is.na(data)) > 0.8), ]

#rename collumn type of data collection because of syntax error in the mice
data <- data %>% 
  rename(data_collection = 'Type of data collection')

#recode variables according to measures manual
data <- data %>% 
  mutate_at(c("BFI44_3", "BFI44_4", "BFI44_9", "BFI44_10", "BFI44_13", "BFI44_14",
              "RRQ_6", "RRQ_9", "RRQ_10"), 
            funs(recode(.,  `1`= 5, `2`=4, `3`=3, `4`=2, `5`=1))) %>%
  mutate_at("SHS_4", funs(recode(., `1`=7, `2`=6, `3`=5, `4`=4, `5`=3, `6`=2, `7`=1))) 

#imputation of missing
#object MICE
init <-   mice(data, maxit=0) 
meth <-   init$method
predM <-   init$predictorMatrix

predM[,c("Age", "Gender", "Status", "Education", "data_collection")] <-  0 #exclusion from the prediction
predM[c("Age", "Gender", "Status", "Education", "data_collection"),] <-  0 #exclusion from the prediction

meth[c("Gender", "Status", "Education", "data_collection")] <-  "" #exclusion from the imputation

set.seed(123)
data_imp <- mice(data, method=meth, predictorMatrix=predM, m=15, maxit = 10) #imputation of missing data - imputed object for the analysis

#convert mice object to list
data_list <- miceadds::mids2datlist(data_imp)

#####preparing total score of the variables on the list created from the 15 imputed variables

### total score and subscales 

#total score maximizing as a goal
data_list <- lapply(data_list, function(x){cbind(x, maximizing_goal = rowSums(x[,c("MTS7_1", "MTS7_2", "MTS7_3", "MTS7_4",
                                                                             "MTS7_5", "MTS7_6", "MTS7_7")], na.rm = TRUE))})
#total score maximizing as a strategy
data_list <- lapply(data_list, function(x){cbind(x, maximizing_strategy = rowSums(x[,c("MI_as_1", "MI_as_2", "MI_as_3", "MI_as_4",
                                                                                 "MI_as_5", "MI_as_6", "MI_as_7", "MI_as_8",
                                                                                 "MI_as_9", "MI_as_10", "MI_as_11", "MI_as_12")], na.rm = TRUE))})
#total score satisficing
data_list <- lapply(data_list, function(x){cbind(x, satisficing = rowSums(x[,c("DMTI_las_1", "DMTI_las_2", "DMTI_las_3", "DMTI_las_4")], na.rm = TRUE))})

#total score extraversion
data_list <- lapply(data_list, function(x){cbind(x, extraversion = rowSums(x[,c("BFI44_1", "BFI44_3", "BFI44_5",
                                                                          "BFI44_7", "BFI44_9", "BFI44_11", "BFI44_13",
                                                                          "BFI44_15")], na.rm = TRUE))})

#total score neuroticism
data_list <- lapply(data_list, function(x){cbind(x, neuroticism = rowSums(x[,c("BFI44_2", "BFI44_4", "BFI44_6",
                                                                         "BFI44_8", "BFI44_10", "BFI44_12", "BFI44_14",
                                                                         "BFI44_16")], na.rm = TRUE))})
#total score happiness
data_list <- lapply(data_list, function(x){cbind(x, happiness = rowSums(x[,c("SHS_1", "SHS_2", "SHS_3", "SHS_4")], na.rm = TRUE))})

#total score depression
data_list <- lapply(data_list, function(x){cbind(x, depression = rowSums(x[,c("BDI2_1", "BDI2_2", "BDI2_3",
                                                                        "BDI2_4", "BDI2_5", "BDI2_6", "BDI2_7",
                                                                        "BDI2_8", "BDI2_9", "BDI2_10", "BDI2_11",
                                                                        "BDI2_12", "BDI2_13", "BDI2_14", "BDI2_15",
                                                                        "BDI2_16", "BDI2_17", "BDI2_18", "BDI2_19",
                                                                        "BDI2_20", "BDI2_21")], na.rm = TRUE))})
#total score self_rumination
data_list <- lapply(data_list, function(x){cbind(x, self_rumination = rowSums(x[,c("RRQ_1", "RRQ_2", "RRQ_3",
                                                                        "RRQ_4", "RRQ_5", "RRQ_6", "RRQ_7",
                                                                        "RRQ_8", "RRQ_9", "RRQ_10", "RRQ_11",
                                                                        "RRQ_12")], na.rm = TRUE))})
####reliability

#total score maximizing as a goal
lapply(data_list, function(x){omega(x[,c("MTS7_1", "MTS7_2", "MTS7_3", "MTS7_4",
                                                "MTS7_5", "MTS7_6", "MTS7_7")])})
#total score maximizing as a strategy
lapply(data_list, function(x){omega(x[,c("MI_as_1", "MI_as_2", "MI_as_3", "MI_as_4",
                                                "MI_as_5", "MI_as_6", "MI_as_7", "MI_as_8",
                                                "MI_as_9", "MI_as_10", "MI_as_11", "MI_as_12")])})
#total score satisficing
lapply(data_list, function(x){omega(x[,c("DMTI_las_1", "DMTI_las_2", "DMTI_las_3", "DMTI_las_4")])})

#total score extraversion
lapply(data_list, function(x){omega(x[,c("BFI44_1", "BFI44_3", "BFI44_5",
                                                "BFI44_7", "BFI44_9", "BFI44_11", "BFI44_13",
                                                "BFI44_15")])})

#total score neuroticism
lapply(data_list, function(x){omega(x[,c("BFI44_2", "BFI44_4", "BFI44_6",
                                                "BFI44_8", "BFI44_10", "BFI44_12", "BFI44_14",
                                                "BFI44_16")])})
#total score happiness
lapply(data_list, function(x){omega(x[,c("SHS_1", "SHS_2", "SHS_3", "SHS_4")])})

#total score depression
lapply(data_list, function(x){omega(x[,c("BDI2_1", "BDI2_2", "BDI2_3",
                                                "BDI2_4", "BDI2_5", "BDI2_6", "BDI2_7",
                                                "BDI2_8", "BDI2_9", "BDI2_10", "BDI2_11",
                                                "BDI2_12", "BDI2_13", "BDI2_14", "BDI2_15",
                                                "BDI2_16", "BDI2_17", "BDI2_18", "BDI2_19",
                                                 "BDI2_20", "BDI2_21")])})
#total score self_rumination
lapply(data_list, function(x){omega(x[,c("RRQ_1", "RRQ_2", "RRQ_3",
                                                "RRQ_4", "RRQ_5", "RRQ_6", "RRQ_7",
                                                "RRQ_8", "RRQ_9", "RRQ_10", "RRQ_11",
                                                "RRQ_12")])})

#######transform list back to MICE object
data_imp <- miceadds::datlist2mids(data_list)

#### descriptives (pooled)

#maximizing as a goal
maximizing_goal_descriptive <- with(data_imp, expr=c("maximizing_goal(mean)"=mean(maximizing_goal), 
                                                     "maximizing_goal(SD)"=stats::sd(maximizing_goal), 
                                                     "maximizing_goal(S.E)"=sd(maximizing_goal)/sqrt(length(maximizing_goal)), 
                                                     "maximizing_goal(min)"=min(maximizing_goal), 
                                                     "maximizing_goal(max)"=max(maximizing_goal)))
# pool estimates
withPool_MI(maximizing_goal_descriptive)

#maximizing as a strategy
maximizing_strategy_descriptive <- with(data_imp, expr=c("maximizing_strategy(mean)"=mean(maximizing_strategy), 
                                                         "maximizing_strategy(SD)"=stats::sd(maximizing_strategy), 
                                                         "maximizing_strategy(S.E)"=sd(maximizing_strategy)/sqrt(length(maximizing_strategy)),
                                                         "maximizing_strategy(min)"=min(maximizing_strategy), 
                                                         "maximizing_strategy(max)"=max(maximizing_strategy)))
# pool estimates
withPool_MI(maximizing_strategy_descriptive)

#satisficing
satisficing_descriptive <- with(data_imp, expr=c("satisficing(mean)"=mean(satisficing), 
                                                 "satisficing(SD)"=stats::sd(satisficing), 
                                                 "satisficing(S.E)"=sd(satisficing)/sqrt(length(satisficing)),
                                                 "satisficing(min)"=min(satisficing), 
                                                 "satisficing(max)"=max(satisficing)))
# pool estimates
withPool_MI(satisficing_descriptive)

#extraversion
extraversion_descriptive <- with(data_imp, expr=c("extraversion(mean)"=mean(extraversion), 
                                                  "extraversion(SD)"=stats::sd(extraversion), 
                                                  "extraversion(S.E)"=sd(extraversion)/sqrt(length(extraversion)),
                                                  "extraversion(min)"=min(extraversion), 
                                                  "extraversion(max)"=max(extraversion)))
# pool estimates
withPool_MI(extraversion_descriptive)

#neuroticism
neuroticism_descriptive <- with(data_imp, expr=c("neuroticism(mean)"=mean(neuroticism), 
                                                 "neuroticism(SD)"=stats::sd(neuroticism), 
                                                 "neuroticism(S.E)"=sd(neuroticism)/sqrt(length(neuroticism)),
                                                 "neuroticism(min)"=min(neuroticism), 
                                                 "neuroticism(max)"=max(neuroticism)))
# pool estimates
withPool_MI(neuroticism_descriptive)

#happiness
happiness_descriptive <- with(data_imp, expr=c("happiness(mean)"=mean(happiness), 
                                               "happiness(SD)"=stats::sd(happiness), 
                                               "happiness(S.E)"=sd(happiness)/sqrt(length(happiness)),
                                               "happiness(min)"=min(happiness), 
                                               "happiness(max)"=max(happiness)))
# pool estimates
withPool_MI(happiness_descriptive)

#depression
depression_descriptive <- with(data_imp, expr=c("depression(mean)"=mean(depression), 
                                                "depression(SD)"=stats::sd(depression), 
                                                "depression(S.E)"=sd(depression)/sqrt(length(depression)),
                                                "depression(min)"=min(depression), 
                                                "depression(max)"=max(depression)))
# pool estimates
withPool_MI(depression_descriptive)

#self_rumination
self_rumination_descriptive <- with(data_imp, expr=c("self_rumination(mean)"=mean(self_rumination), 
                                                     "self_rumination(SD)"=stats::sd(self_rumination), 
                                                     "self_rumination(S.E)"=sd(self_rumination)/sqrt(length(self_rumination)),
                                                     "self_rumination(min)"=min(self_rumination), 
                                                     "self_rumination(max)"=max(self_rumination)))
# pool estimates
withPool_MI(self_rumination_descriptive)

###description of sample and type of data collection
describe(data$Age)
table(data$Gender) 
table(data$Status)
table(data$Education)
table(data$data_collection)

####correlations#### (pooled)

miceadds::micombine.cor(data_imp, 
              variables = c(82,83,84,85,86,87,88,89), 
              conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

#complete all imputed dataset after lapply and create average values for variables from 15 imputed dataset (complete dataset according to Rubin's rule - Rubin (1987))
imp_complete <- mice::complete(data_imp, "long")
#maximizing_goal
imput1 <- imp_complete[1:480, "maximizing_goal"]
imput2 <- imp_complete[481:960, "maximizing_goal"]
imput3 <- imp_complete[961:1440, "maximizing_goal"]
imput4 <- imp_complete[1441:1920, "maximizing_goal"]
imput5 <- imp_complete[1921:2400, "maximizing_goal"]
imput6 <- imp_complete[2401:2880, "maximizing_goal"]
imput7 <- imp_complete[2881:3360, "maximizing_goal"]
imput8 <- imp_complete[3361:3840, "maximizing_goal"]
imput9 <- imp_complete[3841:4320, "maximizing_goal"]
imput10 <- imp_complete[4321:4800, "maximizing_goal"]
imput11 <- imp_complete[4801:5280, "maximizing_goal"]
imput12 <- imp_complete[5281:5760, "maximizing_goal"]
imput13 <- imp_complete[5761:6240, "maximizing_goal"]
imput14 <- imp_complete[6241:6720, "maximizing_goal"]
imput15 <- imp_complete[6721:7200, "maximizing_goal"]

data$maximizing_goal <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                     imput14 + imput15)/15, digits = 0)
#maximizing_strategy
imput1 <- imp_complete[1:480, "maximizing_strategy"]
imput2 <- imp_complete[481:960, "maximizing_strategy"]
imput3 <- imp_complete[961:1440, "maximizing_strategy"]
imput4 <- imp_complete[1441:1920, "maximizing_strategy"]
imput5 <- imp_complete[1921:2400, "maximizing_strategy"]
imput6 <- imp_complete[2401:2880, "maximizing_strategy"]
imput7 <- imp_complete[2881:3360, "maximizing_strategy"]
imput8 <- imp_complete[3361:3840, "maximizing_strategy"]
imput9 <- imp_complete[3841:4320, "maximizing_strategy"]
imput10 <- imp_complete[4321:4800, "maximizing_strategy"]
imput11 <- imp_complete[4801:5280, "maximizing_strategy"]
imput12 <- imp_complete[5281:5760, "maximizing_strategy"]
imput13 <- imp_complete[5761:6240, "maximizing_strategy"]
imput14 <- imp_complete[6241:6720, "maximizing_strategy"]
imput15 <- imp_complete[6721:7200, "maximizing_strategy"]

data$maximizing_strategy <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                                 imput14 + imput15)/15, digits = 0)
#satisficing
imput1 <- imp_complete[1:480, "satisficing"]
imput2 <- imp_complete[481:960, "satisficing"]
imput3 <- imp_complete[961:1440, "satisficing"]
imput4 <- imp_complete[1441:1920, "satisficing"]
imput5 <- imp_complete[1921:2400, "satisficing"]
imput6 <- imp_complete[2401:2880, "satisficing"]
imput7 <- imp_complete[2881:3360, "satisficing"]
imput8 <- imp_complete[3361:3840, "satisficing"]
imput9 <- imp_complete[3841:4320, "satisficing"]
imput10 <- imp_complete[4321:4800, "satisficing"]
imput11 <- imp_complete[4801:5280, "satisficing"]
imput12 <- imp_complete[5281:5760, "satisficing"]
imput13 <- imp_complete[5761:6240, "satisficing"]
imput14 <- imp_complete[6241:6720, "satisficing"]
imput15 <- imp_complete[6721:7200, "satisficing"]

data$satisficing <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                                 imput14 + imput15)/15, digits = 0)
#extraversion
imput1 <- imp_complete[1:480, "extraversion"]
imput2 <- imp_complete[481:960, "extraversion"]
imput3 <- imp_complete[961:1440, "extraversion"]
imput4 <- imp_complete[1441:1920, "extraversion"]
imput5 <- imp_complete[1921:2400, "extraversion"]
imput6 <- imp_complete[2401:2880, "extraversion"]
imput7 <- imp_complete[2881:3360, "extraversion"]
imput8 <- imp_complete[3361:3840, "extraversion"]
imput9 <- imp_complete[3841:4320, "extraversion"]
imput10 <- imp_complete[4321:4800, "extraversion"]
imput11 <- imp_complete[4801:5280, "extraversion"]
imput12 <- imp_complete[5281:5760, "extraversion"]
imput13 <- imp_complete[5761:6240, "extraversion"]
imput14 <- imp_complete[6241:6720, "extraversion"]
imput15 <- imp_complete[6721:7200, "extraversion"]

data$extraversion <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                              imput14 + imput15)/15, digits = 0)
#neuroticism
imput1 <- imp_complete[1:480, "neuroticism"]
imput2 <- imp_complete[481:960, "neuroticism"]
imput3 <- imp_complete[961:1440, "neuroticism"]
imput4 <- imp_complete[1441:1920, "neuroticism"]
imput5 <- imp_complete[1921:2400, "neuroticism"]
imput6 <- imp_complete[2401:2880, "neuroticism"]
imput7 <- imp_complete[2881:3360, "neuroticism"]
imput8 <- imp_complete[3361:3840, "neuroticism"]
imput9 <- imp_complete[3841:4320, "neuroticism"]
imput10 <- imp_complete[4321:4800, "neuroticism"]
imput11 <- imp_complete[4801:5280, "neuroticism"]
imput12 <- imp_complete[5281:5760, "neuroticism"]
imput13 <- imp_complete[5761:6240, "neuroticism"]
imput14 <- imp_complete[6241:6720, "neuroticism"]
imput15 <- imp_complete[6721:7200, "neuroticism"]

data$neuroticism <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                              imput14 + imput15)/15, digits = 0)
#happiness
imput1 <- imp_complete[1:480, "happiness"]
imput2 <- imp_complete[481:960, "happiness"]
imput3 <- imp_complete[961:1440, "happiness"]
imput4 <- imp_complete[1441:1920, "happiness"]
imput5 <- imp_complete[1921:2400, "happiness"]
imput6 <- imp_complete[2401:2880, "happiness"]
imput7 <- imp_complete[2881:3360, "happiness"]
imput8 <- imp_complete[3361:3840, "happiness"]
imput9 <- imp_complete[3841:4320, "happiness"]
imput10 <- imp_complete[4321:4800, "happiness"]
imput11 <- imp_complete[4801:5280, "happiness"]
imput12 <- imp_complete[5281:5760, "happiness"]
imput13 <- imp_complete[5761:6240, "happiness"]
imput14 <- imp_complete[6241:6720, "happiness"]
imput15 <- imp_complete[6721:7200, "happiness"]

data$happiness <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                             imput14 + imput15)/15, digits = 0)
#depression
imput1 <- imp_complete[1:480, "depression"]
imput2 <- imp_complete[481:960, "depression"]
imput3 <- imp_complete[961:1440, "depression"]
imput4 <- imp_complete[1441:1920, "depression"]
imput5 <- imp_complete[1921:2400, "depression"]
imput6 <- imp_complete[2401:2880, "depression"]
imput7 <- imp_complete[2881:3360, "depression"]
imput8 <- imp_complete[3361:3840, "depression"]
imput9 <- imp_complete[3841:4320, "depression"]
imput10 <- imp_complete[4321:4800, "depression"]
imput11 <- imp_complete[4801:5280, "depression"]
imput12 <- imp_complete[5281:5760, "depression"]
imput13 <- imp_complete[5761:6240, "depression"]
imput14 <- imp_complete[6241:6720, "depression"]
imput15 <- imp_complete[6721:7200, "depression"]

data$depression <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                           imput14 + imput15)/15, digits = 0)
#self_rumination
imput1 <- imp_complete[1:480, "self_rumination"]
imput2 <- imp_complete[481:960, "self_rumination"]
imput3 <- imp_complete[961:1440, "self_rumination"]
imput4 <- imp_complete[1441:1920, "self_rumination"]
imput5 <- imp_complete[1921:2400, "self_rumination"]
imput6 <- imp_complete[2401:2880, "self_rumination"]
imput7 <- imp_complete[2881:3360, "self_rumination"]
imput8 <- imp_complete[3361:3840, "self_rumination"]
imput9 <- imp_complete[3841:4320, "self_rumination"]
imput10 <- imp_complete[4321:4800, "self_rumination"]
imput11 <- imp_complete[4801:5280, "self_rumination"]
imput12 <- imp_complete[5281:5760, "self_rumination"]
imput13 <- imp_complete[5761:6240, "self_rumination"]
imput14 <- imp_complete[6241:6720, "self_rumination"]
imput15 <- imp_complete[6721:7200, "self_rumination"]

data$self_rumination<- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                            imput14 + imput15)/15, digits = 0)

#####################################
#######Supplementary analysis######## this analysis is reported as exploratory analysis as suggestion for further research
#####################################

#assumptions for regressions

##target regression: depression ~ neuroticism + extraversion + satisficing + maximizing_goal + maximizing_strategy + self_rumination

##linearity

summary(lm(depression ~ neuroticism, data = data))
summary(lm(depression ~ neuroticism, data = data))$r.squared
summary(lm(depression ~ extraversion, data = data))
summary(lm(depression ~ extraversion, data = data))$r.squared
summary(lm(depression ~ satisficing, data = data))
summary(lm(depression ~ satisficing, data = data))$r.squared
summary(lm(depression ~ maximizing_goal, data = data))
summary(lm(depression ~ maximizing_goal, data = data))$r.squared
summary(lm(depression ~ maximizing_strategy, data = data))
summary(lm(depression ~ maximizing_strategy, data = data))$r.squared
summary(lm(depression ~ self_rumination, data = data))
summary(lm(depression ~ self_rumination, data = data))$r.squared

##multicolinearity

#VIF function: https://rpubs.com/seriousstats/vif

VIF <- function(linear.model, no.intercept=FALSE, all.diagnostics=FALSE, 
                plot=FALSE) {
  require(mctest)
  if(no.intercept==FALSE) design.matrix <- model.matrix(linear.model)[,-1]
  if(no.intercept==TRUE) design.matrix <- model.matrix(linear.model)
  if(plot==TRUE) mc.plot(design.matrix,linear.model$model[1])
  if(all.diagnostics==FALSE) output <- imcdiag(design.matrix,linear.model$model[1], 
                                               method='VIF')$idiags[,1]
  if(all.diagnostics==TRUE) output <- imcdiag(design.matrix,linear.model$model[1])
  output
}

VIF(lm(neuroticism ~ extraversion + satisficing + maximizing_goal + 
         maximizing_strategy + self_rumination, 
       data = data), all.diagnostics = TRUE)
VIF(lm(extraversion ~ neuroticism + satisficing + maximizing_goal + 
         maximizing_strategy + self_rumination, 
       data = data), all.diagnostics = TRUE)
VIF(lm(satisficing ~ neuroticism + extraversion + maximizing_goal + 
         maximizing_strategy + self_rumination, 
       data = data), all.diagnostics = TRUE)
VIF(lm(maximizing_goal ~ neuroticism + extraversion + satisficing + 
         maximizing_strategy + self_rumination, 
       data = data), all.diagnostics = TRUE)
VIF(lm(maximizing_strategy ~ neuroticism + extraversion + satisficing + 
         maximizing_goal + self_rumination, 
       data = data), all.diagnostics = TRUE)
VIF(lm(self_rumination ~ neuroticism + extraversion + satisficing + 
         maximizing_goal + maximizing_strategy, 
       data = data), all.diagnostics = TRUE)

##target regression: happiness ~ neuroticism + extraversion + satisficing + maximizing_goal + maximizing_strategy + self_rumination

##linearity

summary(lm(happiness ~ neuroticism, data = data))
summary(lm(happiness ~ neuroticism, data = data))$r.squared
summary(lm(happiness ~ extraversion, data = data))
summary(lm(happiness ~ extraversion, data = data))$r.squared
summary(lm(happiness ~ satisficing, data = data))
summary(lm(happiness ~ satisficing, data = data))$r.squared
summary(lm(happiness ~ maximizing_goal, data = data))
summary(lm(happiness ~ maximizing_goal, data = data))$r.squared
summary(lm(happiness ~ maximizing_strategy, data = data))
summary(lm(happiness ~ maximizing_strategy, data = data))$r.squared
summary(lm(happiness ~ self_rumination, data = data))
summary(lm(happiness ~ self_rumination, data = data))$r.squared

##multicolinearity

VIF(lm(neuroticism ~ extraversion + satisficing + maximizing_goal + 
         maximizing_strategy + self_rumination, data = data),
    all.diagnostics = TRUE)
VIF(lm(extraversion ~ neuroticism + satisficing + maximizing_goal + 
         maximizing_strategy + self_rumination, data = data),
    all.diagnostics = TRUE)
VIF(lm(satisficing ~ neuroticism + extraversion + maximizing_goal + 
         maximizing_strategy + self_rumination, data = data),
    all.diagnostics = TRUE)
VIF(lm(maximizing_goal ~ neuroticism + extraversion + satisficing + 
         maximizing_strategy + self_rumination, data = data),
    all.diagnostics = TRUE)
VIF(lm(maximizing_strategy ~ neuroticism + extraversion + satisficing + 
         maximizing_goal + self_rumination, data = data),
    all.diagnostics = TRUE)
VIF(lm(self_rumination ~ neuroticism + extraversion + satisficing + 
         maximizing_goal + maximizing_strategy, data = data),
    all.diagnostics = TRUE)

###################################################
### hierarchical linear regression - depression ###
###################################################

block1 <- lm(depression ~ satisficing + maximizing_strategy + maximizing_goal, data = data)
summary(block1)
summary(block1_beta <- lm.beta(block1))

block2 <- update(block1, . ~ . + Age + Gender + neuroticism + extraversion + self_rumination)
summary(block2)
summary(block2_beta <- lm.beta(block2))

block3 <- update(block2, . ~ . + satisficing:neuroticism + 
                   maximizing_strategy:neuroticism + maximizing_goal:neuroticism +
                   satisficing:extraversion + maximizing_strategy:extraversion + 
                   maximizing_goal:extraversion + satisficing:self_rumination + 
                   maximizing_strategy:self_rumination + 
                   maximizing_goal:self_rumination)
summary(block3)
summary(block3_beta <- lm.beta(block3))

anova(block1,block2,block3)

#moderation: depression ~ maximizing_goal*self_rumination 

plotCurves(block3, plotx="maximizing_goal", modx="self_rumination", modxVals="std.dev.",
           col = c("blue", "black", "orange"),
           interval="confidence", cex.main = 0.95,
           main = "Moderation effect of self-rumination on the relationship of maximizing (goal) and depression")

#moderation: depression ~ maximizing_strategy*neuroticism

plotCurves(block3, plotx="maximizing_strategy", modx="neuroticism", modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.92,
           interval="confidence", main = "Moderation effect of neuroticism on the relationship of maximizing (strategy) and depression")

###################################################
### hierarchical linear regression - happiness ###
###################################################

block1a <- lm(happiness ~ satisficing + maximizing_strategy + maximizing_goal, data = data)
summary(block1a)
summary(block1a_beta <- lm.beta(block1a))

block2a <- update(block1a, . ~ . + Age + Gender + neuroticism + extraversion + self_rumination)
summary(block2a)
summary(block2a_beta <- lm.beta(block2a))

block3a <- update(block2a, . ~ . + satisficing:neuroticism + 
                   maximizing_strategy:neuroticism + maximizing_goal:neuroticism +
                   satisficing:extraversion + maximizing_strategy:extraversion + 
                   maximizing_goal:extraversion + satisficing:self_rumination + 
                   maximizing_strategy:self_rumination + 
                   maximizing_goal:self_rumination)
summary(block3a)
summary(block3a_beta <- lm.beta(block3a))

anova(block1a,block2a,block3a)

#moderation: happiness ~ satisficing*neuroticism

plotCurves(block3a, plotx="satisficing", modx="neuroticism", modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.95,
           interval="confidence", main = "Moderation effect of neuroticism on the relationship of satisficing and happiness")

#moderation: happiness ~ maximizing_goal*self_rumination

plotCurves(block3a, plotx="maximizing_goal", modx="self_rumination", 
           modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.95,
           interval="confidence", main = "Moderation effect of self-rumination on the relationship of maximizing (goal) and happiness")

#moderation: happiness ~ maximizing_goal*self_rumination

plotCurves(block3a, plotx="maximizing_strategy", modx="extraversion", 
           modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.95,
           interval="confidence", main = "Moderation effect of extraversion on the relationship of maximizing (strategy) and happiness")


####alternative hypothesis as a suggestions in the discussion

figure5 <- plotCurves(block3, plotx="neuroticism", modx="maximizing_strategy", modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.90,
           interval="confidence", main = "Moderation effect of maximizing (strategy) on the relationship of neuroticism and depression")

figure6 <- plotCurves(block3, plotx="self_rumination", modx="maximizing_goal", modxVals="std.dev.",
           col = c("blue", "black", "orange"),
           interval="confidence", cex.main = 0.90,
           main = "Moderation effect of maximizing (goal) on the relationship of self-rumination and depression")

figure7 <- plotCurves(block3a, plotx="neuroticism", modx="satisficing", modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.90,
           interval="confidence", main = "Moderation effect of satisficing on the relationship of neuroticism and happiness")

figure8 <- plotCurves(block3a, plotx="self_rumination", modx="maximizing_goal", 
           modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.90,
           interval="confidence", main = "Moderation effect of maximizing (goal) on the relationship of self-rumination and happiness")

figure9 <- plotCurves(block3a, plotx="extraversion", modx="maximizing_strategy", 
                      modxVals="std.dev.",
                      col = c("blue", "black", "orange"), cex.main = 0.95,
                      interval="confidence", main = "Moderation effect of extraversion on the relationship of maximizing (strategy) and happiness")
