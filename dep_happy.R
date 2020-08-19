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

#####missing values
sum(is.na(data))/prod(dim(data))
colMeans(is.na(data))
md.pattern(data) 

##remove cases with more than 80% NA
data <- data[which(rowMeans(!is.na(data)) > 0.8), ]

#####missing values
sum(is.na(data))/prod(dim(data))
colMeans(is.na(data))
md.pattern(data) 

###frequencies of sample characteristics
table(data$Gender)
table(data$Status)
table(data$Education)
table(data$data_collection)

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
data_imp <- mice(data, method=meth, predictorMatrix=predM, m=5, maxit = 10) #imputation of missing data - imputed object for the analysis

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

#reliability of maximizing as a goal
lapply(data_list, function(x){omega(x[,c("MTS7_1", "MTS7_2", "MTS7_3", "MTS7_4",
                                                "MTS7_5", "MTS7_6", "MTS7_7")])})
#reliability of maximizing as a strategy
lapply(data_list, function(x){omega(x[,c("MI_as_1", "MI_as_2", "MI_as_3", "MI_as_4",
                                                "MI_as_5", "MI_as_6", "MI_as_7", "MI_as_8",
                                                "MI_as_9", "MI_as_10", "MI_as_11", "MI_as_12")])})
#reliability of satisficing
lapply(data_list, function(x){omega(x[,c("DMTI_las_1", "DMTI_las_2", "DMTI_las_3", "DMTI_las_4")])})

#reliability of extraversion
lapply(data_list, function(x){omega(x[,c("BFI44_1", "BFI44_3", "BFI44_5",
                                                "BFI44_7", "BFI44_9", "BFI44_11", "BFI44_13",
                                                "BFI44_15")])})

#reliability of neuroticism
lapply(data_list, function(x){omega(x[,c("BFI44_2", "BFI44_4", "BFI44_6",
                                                "BFI44_8", "BFI44_10", "BFI44_12", "BFI44_14",
                                                "BFI44_16")])})
#reliability of happiness
lapply(data_list, function(x){omega(x[,c("SHS_1", "SHS_2", "SHS_3", "SHS_4")])})

#reliability of depression
lapply(data_list, function(x){omega(x[,c("BDI2_1", "BDI2_2", "BDI2_3",
                                                "BDI2_4", "BDI2_5", "BDI2_6", "BDI2_7",
                                                "BDI2_8", "BDI2_9", "BDI2_10", "BDI2_11",
                                                "BDI2_12", "BDI2_13", "BDI2_14", "BDI2_15",
                                                "BDI2_16", "BDI2_17", "BDI2_18", "BDI2_19",
                                                 "BDI2_20", "BDI2_21")])})
#reliability of self_rumination
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
imput1 <- imp_complete[1:514, "maximizing_goal"]
imput2 <- imp_complete[515:1028, "maximizing_goal"]
imput3 <- imp_complete[1029:1542, "maximizing_goal"]
imput4 <- imp_complete[1543:2056, "maximizing_goal"]
imput5 <- imp_complete[2057:2570, "maximizing_goal"]


data$maximizing_goal <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)
#maximizing_strategy
imput1 <- imp_complete[1:514, "maximizing_strategy"]
imput2 <- imp_complete[515:1028, "maximizing_strategy"]
imput3 <- imp_complete[1029:1542, "maximizing_strategy"]
imput4 <- imp_complete[1543:2056, "maximizing_strategy"]
imput5 <- imp_complete[2057:2570, "maximizing_strategy"]

data$maximizing_strategy <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)
#satisficing
imput1 <- imp_complete[1:514, "satisficing"]
imput2 <- imp_complete[515:1028, "satisficing"]
imput3 <- imp_complete[1029:1542, "satisficing"]
imput4 <- imp_complete[1543:2056, "satisficing"]
imput5 <- imp_complete[2057:2570, "satisficing"]

data$satisficing <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)
#extraversion
imput1 <- imp_complete[1:514, "extraversion"]
imput2 <- imp_complete[515:1028, "extraversion"]
imput3 <- imp_complete[1029:1542, "extraversion"]
imput4 <- imp_complete[1543:2056, "extraversion"]
imput5 <- imp_complete[2057:2570, "extraversion"]

data$extraversion <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)
#neuroticism
imput1 <- imp_complete[1:514, "neuroticism"]
imput2 <- imp_complete[515:1028, "neuroticism"]
imput3 <- imp_complete[1029:1542, "neuroticism"]
imput4 <- imp_complete[1543:2056, "neuroticism"]
imput5 <- imp_complete[2057:2570, "neuroticism"]

data$neuroticism <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)
#happiness
imput1 <- imp_complete[1:514, "happiness"]
imput2 <- imp_complete[515:1028, "happiness"]
imput3 <- imp_complete[1029:1542, "happiness"]
imput4 <- imp_complete[1543:2056, "happiness"]
imput5 <- imp_complete[2057:2570, "happiness"]

data$happiness <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)
#depression
imput1 <- imp_complete[1:514, "depression"]
imput2 <- imp_complete[515:1028, "depression"]
imput3 <- imp_complete[1029:1542, "depression"]
imput4 <- imp_complete[1543:2056, "depression"]
imput5 <- imp_complete[2057:2570, "depression"]

data$depression <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)
#self_rumination
imput1 <- imp_complete[1:514, "self_rumination"]
imput2 <- imp_complete[515:1028, "self_rumination"]
imput3 <- imp_complete[1029:1542, "self_rumination"]
imput4 <- imp_complete[1543:2056, "self_rumination"]
imput5 <- imp_complete[2057:2570, "self_rumination"]

data$self_rumination<- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)

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

anova(block2,block3)

apa.reg.table(block3, filename = "Table9_APA.doc", table.number = 2)

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
                      col = c("blue", "black", "orange"), cex.main = 0.90,
                      interval="confidence", main = "Moderation effect of maximizing (strategy) on the relationship of extraversion and happiness")


###additional analysis after reviews

miceadds::micombine.cor(data_imp, 
                        variables = c(25,26,27,28,82,83,85,86,87,88,89), 
                        conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

#DMTI_las_1
imput1 <- imp_complete[1:514, "DMTI_las_1"]
imput2 <- imp_complete[515:1028, "DMTI_las_1"]
imput3 <- imp_complete[1029:1542, "DMTI_las_1"]
imput4 <- imp_complete[1543:2056, "DMTI_las_1"]
imput5 <- imp_complete[2057:2570, "DMTI_las_1"]

data$DMTI_las_1 <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)

#DMTI_las_2
imput1 <- imp_complete[1:514, "DMTI_las_2"]
imput2 <- imp_complete[515:1028, "DMTI_las_2"]
imput3 <- imp_complete[1029:1542, "DMTI_las_2"]
imput4 <- imp_complete[1543:2056, "DMTI_las_2"]
imput5 <- imp_complete[2057:2570, "DMTI_las_2"]

data$DMTI_las_2 <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)

#DMTI_las_3
imput1 <- imp_complete[1:514, "DMTI_las_3"]
imput2 <- imp_complete[515:1028, "DMTI_las_3"]
imput3 <- imp_complete[1029:1542, "DMTI_las_3"]
imput4 <- imp_complete[1543:2056, "DMTI_las_3"]
imput5 <- imp_complete[2057:2570, "DMTI_las_3"]

data$DMTI_las_3 <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)

#DMTI_las_4
imput1 <- imp_complete[1:514, "DMTI_las_4"]
imput2 <- imp_complete[515:1028, "DMTI_las_4"]
imput3 <- imp_complete[1029:1542, "DMTI_las_4"]
imput4 <- imp_complete[1543:2056, "DMTI_las_4"]
imput5 <- imp_complete[2057:2570, "DMTI_las_4"]

data$DMTI_las_4 <- round((imput1 + imput2 + imput3 + imput4 + imput5)/5, digits = 0)

a <- cor(data[,c(25,26,27,28,82,83,85,86,87,88,89)])
corrplot(a)

#adding Bayes factor to correlation
library(BayesFactor)

b <- data[,82:89]

correlationBF(b$maximizing_goal, b$maximizing_strategy)
correlationBF(b$maximizing_goal, b$satisficing)
correlationBF(b$maximizing_goal, b$extraversion)
correlationBF(b$maximizing_goal, b$neuroticism)
correlationBF(b$maximizing_goal, b$happiness)
correlationBF(b$maximizing_goal, b$depression)
correlationBF(b$maximizing_goal, b$self_rumination)
correlationBF(b$maximizing_strategy, b$satisficing)
correlationBF(b$maximizing_strategy, b$extraversion)
correlationBF(b$maximizing_strategy, b$neuroticism)
correlationBF(b$maximizing_strategy, b$happiness)
correlationBF(b$maximizing_strategy, b$depression)
correlationBF(b$maximizing_strategy, b$self_rumination)
correlationBF(b$satisficing, b$extraversion)
correlationBF(b$satisficing, b$neuroticism)
correlationBF(b$satisficing, b$happiness)
correlationBF(b$satisficing, b$depression)
correlationBF(b$satisficing, b$self_rumination)
