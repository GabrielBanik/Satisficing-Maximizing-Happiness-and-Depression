#####The way of making a choice: maximizing and satisficing as predictors of happiness... or depression?####

##load data

library(haven)
data <- read_sav("decisionmaking_happiness_depression.sav")

#load packages

library(psych)
library(mctest)
library(rockchalk)
library(apaTables)
library(lm.beta)

#descriptives

data_describe<- subset (data, select = c(age, maximizing_goal, maximizing_strategy, 
                                   satisficing, extraversion, neuroticism, 
                                   happiness, depression, self_rumination))
describe (data_describe)

table(data$gender) # 0 = males; 1 = females

#transforme gender for ploting slopes

data$gender <- as.numeric(as.character(data$gender))

##generate descriptive table

data_describe_cor <- data_describe[,2:9]

apa.cor.table(data_describe_cor, filename="Table1.doc", table.number=1)

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

block2 <- update(block1, . ~ . + age + gender + neuroticism + extraversion + self_rumination)
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
           main = "Moderation effect of self-rumination on relation of maximizing (goal) and depression")

#moderation: depression ~ maximizing_strategy*neuroticism

plotCurves(block3, plotx="maximizing_strategy", modx="neuroticism", modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.92,
           interval="confidence", main = "Moderation effect of neuroticism on relation of maximizing (strategy) and depression")

###################################################
### hierarchical linear regression - happiness ###
###################################################

block1a <- lm(happiness ~ satisficing + maximizing_strategy + maximizing_goal, data = data)
summary(block1a)
summary(block1a_beta <- lm.beta(block1a))

block2a <- update(block1a, . ~ . + age + gender + neuroticism + extraversion + self_rumination)
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
           interval="confidence", main = "Moderation effect of neuroticism on relation of satisficing and happiness")

#moderation: happiness ~ maximizing_goal*self_rumination

plotCurves(block3a, plotx="maximizing_goal", modx="self_rumination", 
           modxVals="std.dev.",
           col = c("blue", "black", "orange"), cex.main = 0.95,
           interval="confidence", main = "Moderation effect of self-rumination on relation of maximizing (goal) and happiness")




