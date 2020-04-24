#####The way of making a choice: maximizing and satisficing as predictors of happiness... or depression?####

##load data

setwd("~/Desktop/Psychologia/Univerzita/projekty/APVV 2018/Výstupy/Články s Lenkou/články - Lenka/dep_happy")
library(haven)
data <- read_sav("decisionmaking_happiness_depression.sav")

#load packages

library(psych)
library(mctest)
library(rockchalk)
library(apaTables)
library(stargazer)
library(lm.beta)

#descriptives

data_describe<- subset (data, select = c(age, maximizing_goal, maximizing_strategy, 
                                   satisficing, extraversion, neuroticism, 
                                   happiness, depression, self_rumination))
describe (data_describe)

table(data$gender) # 0 = males; 1 = females

#transforme gender for ploting slopes

data$gender <- as.numeric(as.character(data$gender))

#assumptions for regression

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

##linear model - depression

dep_lm <- lm(depression ~ age + gender + satisficing + 
                   maximizing_strategy + maximizing_goal + neuroticism +
                   extraversion + self_rumination, data = data)

summary(dep_lm)

summary(dep_lm_beta <- lm.beta(dep_lm))

##linear model - depression - moderation

dep_lm_int <- lm(depression ~ age + gender + satisficing*neuroticism + 
               maximizing_strategy*neuroticism + maximizing_goal*neuroticism +
                  satisficing*extraversion + maximizing_strategy*extraversion + 
               maximizing_goal*extraversion + satisficing*self_rumination + 
               maximizing_strategy*self_rumination + 
               maximizing_goal*self_rumination, data = data)

summary(dep_lm_int)

summary(dep_lm_int_beta <- lm.beta(dep_lm_int))

#moderation: maximizing_goal*self_rumination 

plotCurves(dep_lm_int, plotx="maximizing_goal", modx="self_rumination", modxVals="std.dev.",
           col = c("blue", "black", "orange"),
           interval="confidence", cex.main = 0.95,
           main = "Moderation effect of self-rumination on relation of maximizing (goal) and depression")

#moderation: maximizing_strategy*neuroticism

plotCurves(dep_lm_int, plotx="maximizing_strategy", modx="neuroticism", modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.92,
           interval="confidence", main = "Moderation effect of neuroticism on relation of maximizing (strategy) and depression")

##linear model - depression

happy_lm <- lm(happiness ~ age + gender + satisficing + 
                     maximizing_strategy + maximizing_goal + neuroticism + 
                     extraversion + self_rumination, data = data)

summary(happy_lm)

summary(happy_lm_beta <- lm.beta(happy_lm))

##linear model - depression - moderation

happy_lm_int <- lm(happiness ~ age + gender + satisficing*neuroticism + 
                 maximizing_strategy*neuroticism + maximizing_goal*neuroticism +
                    satisficing*extraversion + maximizing_strategy*extraversion + 
                 maximizing_goal*extraversion + satisficing*self_rumination + 
                 maximizing_strategy*self_rumination + 
                 maximizing_goal*self_rumination, data = data)

summary(happy_lm_int)

##added standardized regression coeficient - beta

summary(lm.beta(happy_lm_int))

#moderation: satisficing*neuroticism

plotCurves(happy_lm_int, plotx="satisficing", modx="neuroticism", modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.95,
           interval="confidence", main = "Moderation effect of neuroticism on relation of satisficing and happiness")

#moderation: maximizing_goal*self_rumination

plotCurves(happy_lm_int, plotx="maximizing_goal", modx="self_rumination", 
           modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.95,
           interval="confidence", main = "Moderation effect of self-rumination on relation of maximizing (goal) and happiness")

##generate tables

data_describe_cor <- data_describe[,2:9]

apa.cor.table(data_describe_cor, filename="Table1.doc", table.number=1)

##linear model - interaction tables

##depression

stargazer(dep_lm, dep_lm_int,type="text",
          column.labels = c("Main Effects", "Interaction"),
          intercept.bottom = FALSE, single.row=TRUE, 
          notes.append = FALSE, header=FALSE)

##happiness

stargazer(happy_lm, happy_lm_int,type="text",
          column.labels = c("Main Effects", "Interactions"),
          intercept.bottom = FALSE, single.row=TRUE, 
          notes.append = FALSE, header=FALSE)

