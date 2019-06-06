#Brent Young
#PREDICT 411
#Moneyball Multiple Linear Regression Project

library(rJava)
library(readr)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(xlsxjars)
library(xlsx)

setwd("~/R/Moneyball")

moneyball=read.csv("moneyball.csv",header=T)

#Part 1: Data Exploration

#Data Quality Check
str(moneyball)
summary(moneyball)

library(Hmisc)
describe(moneyball)

#Wins
par(mfrow=c(1,2))
hist(moneyball$TARGET_WINS, col = "#A71930", xlab = "TARGET_WINS", main = "Histogram of Wins")
boxplot(moneyball$TARGET_WINS, col = "#A71930", main = "Boxplot of Wins")
par(mfrow = c(1,1))
#Batting 
# Hits and Doubles
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", main = "Histogram of Hits")
hist(moneyball$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Doubles")
boxplot(moneyball$TEAM_BATTING_H, col = "#A71930", main = "Boxplot of Hits")
boxplot(moneyball$TEAM_BATTING_2B, col = "#09ADAD", main = "Boxplot of Doubles")
par(mfrow=c(1,1))

# Triples and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", main = "Histogram of Triples")
hist(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", main = "Histogram of Home Runs")
boxplot(moneyball$TEAM_BATTING_3B, col = "#A71930", main = "Boxplot of Triples")
boxplot(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", main = "Boxplot of Home Runs")
par(mfrow=c(1,1))

# Walks, Strikeouts, HBP
par(mfrow=c(2,3))
hist(moneyball$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", main = "Histogram of Walks")
hist(moneyball$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", main = "Histogram of Strikeouts")
hist(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", main = "Histogram of HBP")
boxplot(moneyball$TEAM_BATTING_BB, col = "#A71930", main = "Boxplot of Walks")
boxplot(moneyball$TEAM_BATTING_SO, col = "#09ADAD", main = "Boxplot of Strikeouts")
boxplot(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", main = "Boxplot of HBP")
par(mfrow=c(1,1))

# Stolen Bases and Caught Stealing
par(mfrow=c(2,2))
hist(moneyball$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of Steals")
hist(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", main = "Histogram of CS")
boxplot(moneyball$TEAM_BASERUN_SB, col = "#A71930", main = "Boxplot of Steals")
boxplot(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", main = "Boxplot of CS")
par(mfrow=c(1,1))

#Pitching 
# Hits and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", main = "Histogram of Hits Against")
hist(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", main = "Histograms of HR Against")
boxplot(moneyball$TEAM_PITCHING_H, col = "#A71930", main = "Boxplot of Hits Against")
boxplot(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", main = "Boxplot of HR Against")
par(mfrow=c(1,1))

# Walks and Strikeouts
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", main = "Histogram of Walks Allowed")
hist(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", main = "Histograms of Strikeouts")
boxplot(moneyball$TEAM_PITCHING_BB, col = "#A71930", main = "Boxplot of Walks Allowed")
boxplot(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", main = "Boxplot of Strikeouts")
par(mfrow=c(1,1))

#Fielding
# Double Plays and Errors 
par(mfrow=c(2,2))
hist(moneyball$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", main = "Histogram of Double Plays")
hist(moneyball$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", main = "Histogram of Errors Committed")
boxplot(moneyball$TEAM_FIELDING_DP, col = "#A71930", main = "Boxplot of Double Plays")
boxplot(moneyball$TEAM_FIELDING_E, col = "#09ADAD", main = "Boxplot of Errors Committed")
par(mfrow=c(1,1))

# Scatterplot Matrix 

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Batting Stats and Wins
pairs(moneyball[2:8], lower.panel=panel.smooth, upper.panel = panel.cor)

#Baserunning  Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_FIELDING_DP + moneyball$TEAM_BASERUN_SB, lower.panel = panel.smooth)

#Fielding Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_BASERUN_CS + moneyball$TEAM_FIELDING_E, lower.panel = panel.smooth)

#Pitcher Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_PITCHING_BB + moneyball$TEAM_PITCHING_H + 
        moneyball$TEAM_PITCHING_HR + moneyball$TEAM_PITCHING_SO, lower.panel = panel.smooth)

#Correlation Matrix
subdatnum <- subset(moneyball, select=c(
  "TEAM_BATTING_H",
  "TEAM_BATTING_2B",
  "TEAM_BATTING_3B",
  "TEAM_BATTING_HR",
  "TEAM_BATTING_BB",
  "TEAM_BATTING_SO",
  "TEAM_BASERUN_SB",
  "TEAM_BASERUN_CS",
  "TEAM_BATTING_HBP",
  "TEAM_PITCHING_H",
  "TEAM_PITCHING_HR",
  "TEAM_PITCHING_BB",
  "TEAM_PITCHING_SO",
  "TEAM_FIELDING_E",
  "TEAM_FIELDING_DP",
  "TARGET_WINS"))

require(corrplot)
mcor <- cor(subdatnum)
corrplot(mcor, method="number", shade.col=NA, tl.col="black",tl.cex=0.8)

#Part 2: Data Preparation
library(mice)

#Check for missing values
sapply(moneyball, function(x) sum(is.na(x)))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(moneyball,2,pMiss)

library(VIM)
aggr_plot <- aggr(moneyball, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(moneyball), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

moneyball2 <- subset(moneyball, select=c(
  "TEAM_BATTING_H",
  "TARGET_WINS",
  "TEAM_BATTING_HBP",
  "TEAM_BATTING_2B",
  "TEAM_BATTING_3B",
  "TEAM_BATTING_HR",
  "TEAM_BATTING_BB",
  "TEAM_BATTING_SO",
  "TEAM_BASERUN_SB",
  "TEAM_BASERUN_CS",
  "TEAM_PITCHING_H",
  "TEAM_PITCHING_HR",
  "TEAM_PITCHING_BB",
  "TEAM_PITCHING_SO",
  "TEAM_FIELDING_E",
  "TEAM_FIELDING_DP"))

#Run imputation
tempData <- mice(moneyball2,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

#Check N/A values have been removed
moneyball3 <- complete(tempData,1)
apply(moneyball3,2,pMiss)
summary(moneyball3)

# Inspecting the distribution of original and imputed data for the variables that contained N/A
xyplot(tempData,TARGET_WINS~ TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS +  TEAM_PITCHING_SO + TEAM_FIELDING_DP + TEAM_BATTING_HBP,pch=18,cex=1)

densityplot(tempData)

stripplot(tempData, pch = 20, cex = 1.2)

#Straighten Relationships - Create transformed variables that we can look at later
moneyball3$TEAM_BATTING_1B <- moneyball3$TEAM_BATTING_H - moneyball3 $TEAM_BATTING_HR - moneyball3$TEAM_BATTING_3B - moneyball3$TEAM_BATTING_2B
moneyball3$SB_PCT <- moneyball3$TEAM_BASERUN_SB/(1.0* moneyball3$TEAM_BASERUN_SB+ moneyball3$TEAM_BASERUN_CS)
moneyball3$logTEAM_BATTING_1B <- log(moneyball3$TEAM_BATTING_1B)
moneyball3$logTEAM_BATTING_2B <- log(moneyball3$TEAM_BATTING_2B)
moneyball3$logTEAM_BATTING_3B <- log(moneyball3$TEAM_BATTING_3B)
moneyball3$logTEAM_BATTING_HR<- log(moneyball3$TEAM_BATTING_HR)
moneyball3$logTEAM_BATTING_BB <- log(moneyball3$TEAM_BATTING_BB)
moneyball3$logTEAM_BATTING_SO<- log(moneyball3$TEAM_BATTING_SO)
moneyball3$logTEAM_BASERUN_SB <- log(moneyball3$TEAM_BASERUN_SB)
moneyball3$logTEAM_BASERUN_CS <- log(moneyball3$TEAM_BASERUN_CS)
moneyball3$logTEAM_PITCHING_BB <- log(moneyball3$TEAM_PITCHING_BB)
moneyball3$logTEAM_FIELDING_E <- log(moneyball3$TEAM_FIELDING_E)
moneyball3$logTEAM_FIELDING_DP <- log(moneyball3$TEAM_FIELDING_DP)

#Trim Data
moneyball3$TARGET_WINS[(moneyball3$TARGET_WINS >= 120)] = 120
moneyball3$TARGET_WINS[(moneyball3$TARGET_WINS <= 21)] = 21
moneyball3$TEAM_BATTING_H[(moneyball3$TEAM_BATTING_H >= 2000)] = 2000
moneyball3$TEAM_BATTING_H[(moneyball3$TEAM_BATTING_H <= 1000)] = 1000
moneyball3$TEAM_BATTING_2B[(moneyball3$TEAM_BATTING_2B >= 400)] = 400
moneyball3$TEAM_BATTING_2B[(moneyball3$TEAM_BATTING_2B <= 100)] = 100
moneyball3$TEAM_BATTING_3B[(moneyball3$TEAM_BATTING_3B >= 160)] = 160
moneyball3$TEAM_BATTING_3B[(moneyball3$TEAM_BATTING_3B <= 10)] = 10
moneyball3$TEAM_BATTING_HR[(moneyball3$TEAM_BATTING_HR <= 3)] = 3 
moneyball3$TEAM_BATTING_BB[(moneyball3$TEAM_BATTING_BB >= 825)] = 825
moneyball3$TEAM_BATTING_BB[(moneyball3$TEAM_BATTING_BB <= 280)] = 280
moneyball3$TEAM_BATTING_SO[(moneyball3$TEAM_BATTING_SO >= 300)] = 300
moneyball3$TEAM_BASERUN_SB[(moneyball3$TEAM_BASERUN_SB >= 350)] = 350
moneyball3$TEAM_BASERUN_SB[(moneyball3$TEAM_BASERUN_SB <= 14)] = 14
moneyball3$TEAM_BASERUN_CS[(moneyball3$TEAM_BASERUN_CS >= 125)] = 125
moneyball3$TEAM_BASERUN_CS[(moneyball3$TEAM_BASERUN_CS <= 10)] = 10
moneyball3$TEAM_PITCHING_H[(moneyball3$TEAM_PITCHING_H >= 2000 )] = 2000
moneyball3$TEAM_PITCHING_HR[(moneyball3$TEAM_PITCHING_HR >= 260)] = 260
moneyball3$TEAM_PITCHING_HR[(moneyball3$TEAM_PITCHING_HR <= 25)] = 25
moneyball3$TEAM_PITCHING_BB[(moneyball3$TEAM_PITCHING_BB >= 1000)] = 1000
moneyball3$TEAM_PITCHING_BB[(moneyball3$TEAM_PITCHING_BB <= 300)] = 300
moneyball3$TEAM_PITCHING_SO[(moneyball3$TEAM_PITCHING_SO >= 1550)] = 1550
moneyball3$TEAM_PITCHING_SO[(moneyball3$TEAM_PITCHING_SO <= 100)] = 100
moneyball3$TEAM_FIELDING_E[(moneyball3$TEAM_FIELDING_E >= 500)] = 500
summary(moneyball3)

#Part 3: Model Creation
# Manual Approach

#Correlation Matrix
subdatnum2 <- subset(moneyball3, select=c(
  "TEAM_BATTING_HBP",
  "TEAM_BATTING_H",
  "TEAM_BATTING_2B",
  "TEAM_BATTING_3B",
  "TEAM_BATTING_HR",
  "TEAM_BATTING_BB",
  "TEAM_BATTING_SO",
  "TEAM_BASERUN_SB",
  "TEAM_BASERUN_CS",
  "TEAM_PITCHING_H",
  "TEAM_PITCHING_HR",
  "TEAM_PITCHING_BB",
  "TEAM_PITCHING_SO",
  "TEAM_FIELDING_E",
  "TEAM_FIELDING_DP",
  "TARGET_WINS"))

require(corrplot)
mcor <- cor(subdatnum2)
corrplot(mcor, method="number", shade.col=NA, tl.col="black",tl.cex=0.8)
par(mfrow=c(1,1))  


MLRResult1<- lm(formula = TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BASERUN_SB + TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_FIELDING_E, data = moneyball3)

summary(MLRResult1)
vif(MLRResult1)

MLRResult2<- lm(formula = TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_BB + TEAM_BASERUN_SB + TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_FIELDING_E, data = moneyball3)

anova(MLRResult2)
summary(MLRResult2)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRResult2)
vif(MLRResult2)

# Stepwise Approach

stepwisemodel <- lm(formula = TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + SB_PCT, data = moneyball3)
stepwise <- stepAIC(stepwisemodel, direction = "both")
summary(stepwise)
anova(stepwise)
summary(stepwise)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(stepwise)
vif(stepwise)

#Model 3 

Model3 <- lm(formula = TARGET_WINS ~ logTEAM_BATTING_1B + logTEAM_BATTING_2B + 
               TEAM_BATTING_3B + TEAM_BATTING_BB + TEAM_BATTING_HR + TEAM_BASERUN_SB + TEAM_FIELDING_E + logTEAM_FIELDING_DP, data = moneyball3)

anova(Model3)
summary(Model3)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(Model3)
vif(Model3)

subsets <- regsubsets(TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + 
                        TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_BB +TEAM_FIELDING_E + TEAM_FIELDING_DP,  data = moneyball3, nbest = 2)
plot(subsets, scale="adjr2")

#Part 4: Performance 

#Function for Mean Square Error Calculation
mse <- function(sm) 
  mean(sm$residuals^2)

AIC(MLRResult1)
AIC(MLRResult2)
AIC(stepwisemodel)
AIC(Model3)

BIC(MLRResult1)
BIC(MLRResult2)
BIC(stepwisemodel)
BIC(Model3)

mse(MLRResult1)
mse(MLRResult2)
mse(stepwisemodel)
mse(Model3)

#####
#Designated proper working environment on my computer. You will want to make sure it is in proper place for your computer.
#####

#Part 5: Test Data 
setwd("~/R/Moneyball")
moneyball_test=read.csv("moneyball_test.csv",header=T)

# Fixing na's
library(mice)

#Check for missing values
sapply(moneyball_test, function(x) sum(is.na(x)))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(moneyball_test,2,pMiss)

library(VIM)
aggr_plot <- aggr(moneyball_test, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(moneyball_test), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Run imputation
tempData <- mice(moneyball_test,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

#Check N/A values have been removed
moneyball5  <- complete(tempData,1)
apply(moneyball5 ,2,pMiss)
summary(moneyball5 )

densityplot(tempData)

stripplot(tempData, pch = 20, cex = 1.2)

#Straighten Relationships - Create transformed variables that we can look at later
moneyball5$TEAM_BATTING_1B <- moneyball5$TEAM_BATTING_H - moneyball5$TEAM_BATTING_HR -moneyball5$TEAM_BATTING_3B - moneyball5$TEAM_BATTING_2B
moneyball5$logTEAM_BATTING_1B <- log(moneyball5$TEAM_BATTING_1B)
moneyball5$logTEAM_BATTING_2B <- log(moneyball5$TEAM_BATTING_2B)
moneyball5$logTEAM_FIELDING_DP <- log(moneyball5$TEAM_FIELDING_DP)

#Trim Data
moneyball5$TEAM_BATTING_H[(moneyball5$TEAM_BATTING_H >= 2000)] = 2000
moneyball5$TEAM_BATTING_H[(moneyball5$TEAM_BATTING_H <= 1000)] = 1000
moneyball5$TEAM_BATTING_2B[(moneyball5$TEAM_BATTING_2B >= 400)] = 400
moneyball5$TEAM_BATTING_2B[(moneyball5$TEAM_BATTING_2B <= 100)] = 100
moneyball5$TEAM_BATTING_3B[(moneyball5$TEAM_BATTING_3B >= 160)] = 160
moneyball5$TEAM_BATTING_3B[(moneyball5$TEAM_BATTING_3B <= 10)] = 10
moneyball5$TEAM_BATTING_HR[(moneyball5$TEAM_BATTING_HR <= 3)] = 3 
moneyball5$TEAM_BATTING_BB[(moneyball5$TEAM_BATTING_BB >= 825)] = 825
moneyball5$TEAM_BATTING_BB[(moneyball5$TEAM_BATTING_BB <= 280)] = 280
moneyball5$TEAM_BATTING_SO[(moneyball5$TEAM_BATTING_SO >= 300)] = 300
moneyball5$TEAM_BASERUN_SB[(moneyball5$TEAM_BASERUN_SB >= 350)] = 350
moneyball5$TEAM_BASERUN_SB[(moneyball5$TEAM_BASERUN_SB <= 14)] = 14
moneyball5$TEAM_BASERUN_CS[(moneyball5$TEAM_BASERUN_CS >= 125)] = 125
moneyball5$TEAM_BASERUN_CS[(moneyball5$TEAM_BASERUN_CS <= 10)] = 10
moneyball5$TEAM_PITCHING_H[(moneyball5$TEAM_PITCHING_H >= 2000 )] = 2000
moneyball5$TEAM_PITCHING_HR[(moneyball5$TEAM_PITCHING_HR >= 260)] = 260
moneyball5$TEAM_PITCHING_HR[(moneyball5$TEAM_PITCHING_HR <= 25)] = 25
moneyball5$TEAM_PITCHING_BB[(moneyball5$TEAM_PITCHING_BB >= 1000)] = 1000
moneyball5$TEAM_PITCHING_BB[(moneyball5$TEAM_PITCHING_BB <= 300)] = 300
moneyball5$TEAM_PITCHING_SO[(moneyball5$TEAM_PITCHING_SO >= 1550)] = 1550
moneyball5$TEAM_PITCHING_SO[(moneyball5$TEAM_PITCHING_SO <= 100)] = 100
moneyball5$TEAM_FIELDING_E[(moneyball5$TEAM_FIELDING_E >= 500)] = 500

# Stand Alone Scoring
moneyball5$P_TARGET_WINS <- -2.764e+02 + 
  5.452e+01*moneyball5$logTEAM_BATTING_1B+ 
  5.273e+00*moneyball5$logTEAM_BATTING_2B+
  1.609e-01*moneyball5$TEAM_BATTING_3B +
  8.643e-02*moneyball5$TEAM_BATTING_HR+
  2.138e-02*moneyball5$TEAM_BATTING_BB+
  6.535e-02*moneyball5$TEAM_BASERUN_SB-
  7.216e-02*moneyball5$TEAM_FIELDING_E-
  1.484e+01*moneyball5$logTEAM_FIELDING_DP

#subset of data set for the deliverable "Scored data file"
prediction <- moneyball5[c("INDEX","P_TARGET_WINS")]

#####
#Note, this next function will output an Excel file in your work environment called write.xlsx.
#####

#Prediction File 
write.xlsx(prediction, file = "writeQ.xlsx", sheetName = "Predictions",
           col.names = TRUE)
