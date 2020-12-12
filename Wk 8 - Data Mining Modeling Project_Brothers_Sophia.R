install.packages("tidyverse")
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
install.packages("skimr")
library(skimr)
library(caret)

Bankruptcy = read.csv("C:\\Users\\student\\Documents\\Bankruptcy.csv",header = TRUE, sep = ",")


# Decide on what data mining technique(s) would be appropriate in assessing whether there are groups of variables that 
# convey the same information and how important that information is? Conduct such an analysis.

# Comment in your presentation on the distinct goals of profiling the characteristics of bankrupt firms versus simply predicting
# (black box style) whether a firm will go bankrupt and whether both goals, or only one, might be useful. Also comment on the
# classification methods that would be appropriate in each circumstance.

# Explore the data to gain a preliminary understanding of which variables might be important in distinguishing bankrupt from
# nonbankrupt firms. (Hint: As part of this analysis, use side-by-side boxplots, with the bankrupt/not bankrupt variable as the
# x variable.)

# Using your choice of classifiers, use R to produce several models to predict whether or not a firm goes bankrupt, assessing model 
# performance on a validation partition. Based on the above, comment on which variables are important in classification, and discuss
# their effect.

# VARIABLES
# R1-R4  cash reservoir with which to pay debts.
# R9-R11 current assets with which to pay debts.
# R12 and R21, measure the firm's debt structure. 
# R8 and R19 Inventory and receivables turnover.
# R20 ability to generate sales.
# The final 12 ratios are asset flow measures.



# 1. Explore-----------------------------------------------------------------------------------------

head(Bankruptcy)
str(Bankruptcy)     
summary(Bankruptcy)
skimr::skim(Bankruptcy)   
any(is.na(Bankruptcy))

table(Bankruptcy$D) 
table(Bankruptcy$YR)


# BOX PLOTS  (To distinguish between bankrupt and nonbankrupt)
library(reshape)

# MEASURE OF CASH RESERVOIRS
cash <- melt(Bankruptcy,id.vars='D', measure.vars=c("R1","R2","R3","R4"))
p1 <- ggplot(cash, aes(x=as.factor(D), y=value, fill=D)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  ggtitle("Cash Reservoirs")
p1

# MEASURE OF CURRENT ASSETS
assets <- melt(Bankruptcy,id.vars='D', measure.vars=c("R9","R10","R11"))
p2 <- ggplot(assets, aes(x=as.factor(D), y=value, fill=D)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  ggtitle("Current Assets")
p2

# MEASURE OF DEBT STRUCTURE
debt <-melt(Bankruptcy,id.vars='D', measure.vars=c("R12", "R21"))
p3 <- ggplot(debt, aes(x=as.factor(D), y=value, fill=D)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  ggtitle("Debt Structure")
p3

# MEASURE OF INVENTORY AND RECEIVABLES TURNOVER
turnover <-melt(Bankruptcy,id.vars='D', measure.vars=c("R8", "R19"))
p4 <- ggplot(turnover, aes(x=as.factor(D), y=value, fill=D)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  ggtitle("Invenorty & Receivables Turnover")
p4

# MEASURE OF ABILITY TO GENERATE SALES
sales <-melt(Bankruptcy,id.vars='D', measure.vars=c("R20"))
p5 <- ggplot(sales, aes(x=as.factor(D), y=value, fill=D)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  ggtitle("Sales Generation")
p5

# MEASURE OF ASSET FLOW (1st half)
flow1 <-melt(Bankruptcy,id.vars='D', measure.vars=c("R5","R6","R7","R13","R14","R15"))
p6 <- ggplot(flow1, aes(x=as.factor(D), y=value, fill=D)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  ggtitle("Asset Flow set1")
p6


# MEASURE OF ASSET FLOW (2nd half)
flow2 <-melt(Bankruptcy,id.vars='D', measure.vars=c("R16","R17","R18","R22","R23","R24"))
p7 <- ggplot(flow2, aes(x=as.factor(D), y=value, fill=D)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  ggtitle("Asset Flow set2")
p7


# Based off these boxplots, I conclude that 0 is bankrupt and 1 is non-bankrupt.

# Distinguishing variables are: R3(CASH / ASSETS), R4(CASH / DEBTS), R9(CURASS / CURDEBT), 
# R12(CURDEBT / DEBTS), R21(ASSETS / DEBTS), R20(SALES / ASSETS), R7(CFFO / DEBTS),
# R14(INC / ASSETS), R15(INC / DEBTS), R16(INCDEP / SALES), R17(INCDEP / ASSETS),
# R18(INCEP / DEBTS), R23(WCFO / ASSETS)


# NEW DATA SET  (Distinguished variables only)----

undistiguished <- c("R1","R2","R5","R6","R8","R10","R11","R13","R19","R22","R24")
Bankruptcy.2 <- Bankruptcy[,!(names(Bankruptcy) %in% undistiguished)]



# CORRELATION  
# (>0.7 Indicates multicollinearity.)

# PLOTTING WITH GGPAIRS----
install.packages("GGally")
library(GGally)
ggpairs(Bankruptcy.2[, c("D","R3","R4","R9")])
ggpairs(Bankruptcy.2[, c("D","R12","R21","R20")])
ggpairs(Bankruptcy.2[, c("D","R7","R14","R15")])
ggpairs(Bankruptcy.2[, c("D","R16","R17","R18", "R23")])

# Strong correlation between: R3/R4, R9/R21, R17/R16, R16/R23, R17/R23


# PLOTTING WITH CORRPLOT----
library(corrplot)
correlations <- cor(Bankruptcy.2[, 4:16])
corrplot(correlations, method = "circle")

# More strong relationships found. 
cor.test(Bankruptcy.2$R23, Bankruptcy$R15)   # for testing
# R3: R4
# R9: R21 
# R14: R16, R17, R23
# R15: R18, R21
# R16: R14, R17, R23
# R17: R14, R16, R23
# R18: R15, R21 
# R21: R9, R15, R18
# R23: R14, R16, R17


# CORRELATION HEATMAP----
library(reshape)

BK_cont <- subset(Bankruptcy.2, select = -c(NO,YR, D)) # remove non-continuous variables

BK.Rounded<- round(cor(BK_cont),2)   #create matrix table

melted.BK <- melt(BK.Rounded)  #stack columns into a single column

BK.plot <- ggplot(melted.BK, aes(x = X1, y = X2, fill = value)) +   #create heatmap
  geom_tile() +
  geom_text(aes(x = X1, y = X2,label = value)) +
  theme(axis.text.x = element_text(angle=-45))

install.packages('plotly')   # interactive
library(plotly)

ggplotly(BK.plot)  


# PRINCIPAL COMPONENT ANALYSIS----

# normalize 
pca.nor <- prcomp(na.omit(Bankruptcy.2[,-c(1:3)]), center = T, scale. = T)
summary(pca.nor)    # *PCs that have sd of about 1 can be used. So I can stop at PC4
pca.nor$rot[,1:4]  # rotation matrix. tells how much each variable makes up each PC (weights).
plot(pca.nor)
screeplot(pca.nor, type = "line", main = "Scree Plot") #just another plot 
biplot(pca.nor)

# If I were to choose to predict using PCA, then I would use only PC1 through PC4 as predictors.



# 2. PREPROCESS DATA-------------------------------------------------------------------------------
# keep R4, R9, R16, R18, R21
# Drop "NO" and "YR" as they are not needed.
# Drop variables that may be measuring the same thing.
# (Pick the ones that have the least correlation with the output variable): R3, R12, R16, R18, R20

drop <- c("NO","YR","R3","R7","R12","R15","R17","R20","R23")
BK.reduced <- Bankruptcy.2[,!(names(Bankruptcy.2) %in% drop)]
names(BK.reduced)


# Check for multicollinearity in reduced data set With eigen()
bk<- cor(BK.reduced)
bk

eigen(cor(BK.reduced))$values

max(eigen(cor(BK.reduced))$values) / min(eigen(cor(BK.reduced))$values)  # more than 100 indicates significate multicollinearity problem
kappa(cor(BK.reduced), exact = T)    #you can also use kapp function.


# CONVERT D INTO A FACTOR
BK.reduced$D <- factor(BK.reduced$D)



# PARTITION-----------------------------------------------------------------------------------

set.seed(123)
train.index <- sample(c(1:dim(BK.reduced)[1]), dim(BK.reduced)[1]*0.6)
train.df <- BK.reduced[train.index, ]
valid.df <- BK.reduced[-train.index, ]

options(scipen = 999)


# MODELING (3 DIFFERENT MODELS)-------------------------------------------------------------

# DECESION TREE  wk5      

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("e1071")
library(e1071)



# fitting model.
classtree.m1 <- rpart(D ~ ., data = train.df,  
                      control = rpart.control(maxdepth = 2), method = "class")

#plotting tree with prp()
prp(classtree.m1, type = 2, extra = 1, under= TRUE, split.font = 2, varlen = -10)  
rpart.plot(classtree.m1) #another plot

# Predict
DT.pred.1 <- predict(classtree.m1, train.df, type = "class")
DT.pred.2 <- predict(classtree.m1, valid.df, type = "class")

# Check accuracy
DT.pt1 <- table(DT.pred.1, train.df$D)
confusionMatrix(DT.pt1)     # ACCURACY: 0.8734

DT.pt2 <- table(DT.pred.2, valid.df$D)
confusionMatrix(DT.pt2)     # ACCURACY: 0.6981

#------------------------------------------------------------------------------------------------------------

# LOGISTIC REGRSSION MODEL  wk5

# treat D as categorical (R will create dummy variables)
#BK.reduced$D <- factor(BK.reduced$D)


# use glm() (general linear model) with family = "binomial" to fit a logistic regression.
logit.reg1 <- glm(D ~ ., data = train.df, family = "binomial"(link= 'logit'))
summary(logit.reg1)

# AIC: 55.565
#  A p-value less than 0.05 is statistically significant.

# predict
LR.pred.1 <- predict(logit.reg1, train.df, type = "response")
LR.pred.1
LR.pred.2 <- predict(logit.reg1, valid.df, type = "response")
LR.pred.2

# evaluate
LR.pt1<- table(ifelse(LR.pred.1 > 0.5, 1, 0), train.df$D)
LR.pt1
confusionMatrix(LR.pt1)    # Accuracy: 0.8481

LR.pt2<- table(ifelse(LR.pred.2 > 0.5, 1, 0), valid.df$D)
LR.pt2
confusionMatrix(LR.pt2)   # Accuracy:  0.7358 


# plotting predictions
plot(LR.pred.2, col = "purple")     # shows unclassified values
abline(h=0.5, col="blue")

p = ifelse(LR.pred.2 > 0.5, 1, 0)   # shows classified values
plot(p, col = "purple")
abline(h=0.5, col="blue")


# first 5 actual and predicted records
data.frame(actual = valid.df$D[1:5], predicted = LR.pred.2 [1:5])


library(stats)

train.stepwise_1 <-step(logit.reg1)  
summary(train.stepwise_1)

# AIC 51.062
# Best fit:  glm(formula = D ~ R4 + R9 + R18, family = binomial(link = "logit"), data = train.df)

#--------------------------------------------------------------------------------------------------
# NEURAL NETWORK  wk6

# Remember to first scale the numerical predictor and outcome variables to a 0-1 scale (use function preprocess() with method = "range"-see Chapter 7) 
# and convert categorical predictors to dummies.

library(dummies)

# Converting categorical to dummies. (If already in factor format, it will automatically convert to dummies)
#BK.reduced$D <- factor(BK.reduced$D)   # ALREADY DONE
BK_dummy <- dummy.data.frame(BK.reduced)
summary(BK_dummy)


# normalizing sample data
norm <- preProcess(BK_dummy[,], method= "range")  
norm

# now transforming sample dataset with the normalized values
BK_norm <- predict(norm, BK_dummy[,])
summary(BK_norm)


set.seed(123)
train.index <- as.numeric(sample(c(1:dim(BK.reduced)[1]), dim(BK.reduced)[1]*0.6))  # must be numeric
train.df <- BK.reduced[train.index, ]
valid.df <- BK.reduced[-train.index, ]

# Fitting a model
install.packages("neuralnet")
library(neuralnet)


# 1 LAYER, 2 NODES
nn1 <- neuralnet(D ~ R4 + R9 + R14 + R16 + R18 + R21,
                 data = train.df, hidden = 2, linear.output = FALSE, threshold=0.01 )
plot(nn1)

# predicting
NN.pred.t2 <- compute(nn1, train.df[ ,-1])$net.result
NN.pred.v2 <- compute(nn1, valid.df[ ,-1])$net.result


# Check accuracy
#install.packages("Metrics")
#library(Metrics)
# RMSE  !!!!wont work on factors.
#rmse(train.df[,"D"], NN.pred.t2)   
#rmse(valid.df[,"D"], NN.pred.v2) 


predicted.class.1<- apply(NN.pred.t2,1,which.max)-1
NN.pt1 <- table(ifelse(predicted.class.1 > 0.5, 1, 0), train.df$D)
confusionMatrix(NN.pt1)   # Accuracy : 0.8861 

predicted.class.2<- apply(NN.pred.v2,1,which.max)-1
NN.pt2 <- table(ifelse(predicted.class.2 > 0.5, 1, 0), valid.df$D)
confusionMatrix(NN.pt2)   # Accuracy : 0.7547




# SUMMARY---------------------------------------------------------------------

# NEURAL NETWORK IS THE BEST MODEL

#  Models: 
#                                Test Accuracy            Valid Accuracy 
#  Decision Tree:                  .8734                       .6981
#  Logistic Regression:            .8481                       .7358 
#  Neural Network:                 .8861                       .7547                   


# Predictors:   R4, R9, R14, R16, R18, R21

# 	CASH / DEBTS 
#   CURASS / CURDEBT 
#   INC / ASSETS
#   INCDEP / SALES
#   INCEP / DEBTS
#   ASSETS / DEBTS


#  Other:
#     Par the decision tree, R21 is the greatest predictor of whether a company will bankrupt or not.
#     Par step wise regression, formula = D ~ R4 + R9 + R18, is the best fit for a logistic regression.
#     No insight into how the Neural Network reached its results . This is the black box.