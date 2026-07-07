#------------------------- Msc In Statistics AUEB ---------------------------
#
# Purpose: Final Assignment in Financial Analytics Course. 
# This project involves analyzing the monthly returns of USA mutual funds using various
# financial models and constructing optimal portfolios. The tasks are divided into two main parts:
# 
## Steps of the Assignment:
#
# Part A: Performance Evaluation
# A.Analyze mutual fund returns using data from 7/1963 – 7/2015 (initial estimation period).
# Evaluate the performance of USA mutual funds for the period 8/2015 – 7/2019 (out-of-sample period).
# Construct equally weighted portfolios for the out-of-sample period based on the top 20% selected funds.
# Assess the performance of these funds using various performance measures:
#    a. Sharpe ratio
#    b. Treynor ratio
#    c. Jensen’s alpha
#    d. Sortino ratio
#
# and also estimate Jensen’s alpha using the following models:
#    a. Single factor model (S&P500 return index as the market factor)
#    b. Multiple regression models
#    c. Multiple regression – GARCH type models
#    d. by suggesting an alternative modeling approach. We fitted a GARCH (2,2) and a EGARCH (1,1).
#
# # Part B: Portfolio Construction
#   Construct optimal minimum variance portfolios and mean-variance portfolios.
# 
#   Estimate the mean vector and covariance matrix using the following methods/models:
#    a. Sample estimate of mean vector and covariance matrix
#    b. Single index model
#    c. Multivariate multiple regression models
#    d. Constant Conditional Correlation (CCC) model
#    e. Suggest an alternative modeling approach.We fitted a FAMMA FRENCH 3 FACTOR MODEL and a FAMMA FRENCH 4 FACTOR MODEL
#  In all these, we have evaluated the constructed portfolios based on:
#    a. the Realized returns
#    b. the Cumulative returns
#    c. and the Conditional Sharpe Ratio
#
#  # Data Source:
#   US_FUND_DATA.xls (monthly returns of USA mutual funds and explanatory factors).
# In order someone to run this code he/she will need to download this excel file and replace the 
# file_path with the path that he/she stored this excel file.
#
#
# Author: Konstantinos Grammenos
# 
# Date: 24/6/2024
#
# Proffesor: I.Vrontos


#import the neccessary libraries
rm(list=ls(all=TRUE))
library(timeSeries)
library(readxl)
library(writexl)
library(dplyr)
library(zoo)
library(MASS)
library(fGarch) 
library(Hmisc)
library(rugarch)
library(tseries)
library(quadprog)
library(lmtest)
library(sandwich)
library(ggplot2)
library(dplyr)


par(mfrow = c(1, 1))
#------------------------------------ LOAD THE DATA ----------------------------------------------------
file_path <- "C:\\Users\\kosti\\OneDrive\\Desktop\\Financial Analytics\\Assignment\\US_FUND_DATA.xlsx"
dataUS <- read_excel(file_path, sheet = "US_Funds")
factors <- read_excel(file_path, sheet = "Factors")

# monthly returns of USA mutual funds for the period 7/1963 – 7/2019
Time <- seq(as.Date("1963-07-01"), as.Date("2019-07-01"), by="1 month")
Time <- timeLastDayInMonth(Time)
head(Time)
#check the first 5 
head(dataUS)
dim(dataUS)

#------------------------------------ DATA CLEANING ------------------------------------------
# Convert to dataframe
# take only the series 1-90
dataUS <- as.data.frame(dataUS[, 1:91]) 

# Extract the dates from the first column
dates <- as.Date(dataUS[,1], format="%Y-%m-%d")
# Remove the first column (dates)
dataUS <- dataUS[,-1]  
head(dataUS)
class(dataUS)

# Check the dimensions of our data
dim(dataUS)
dim(factors)
# after checking the dimensions we found out that we have to throw away the 6 last rows
# our dataset should have 673 rows , these are the months from the period 7/1963 – 7/2019

# get rid of the 6 last rows 
dataUS <- dataUS[1:(nrow(dataUS)-6), ]
dates <- dates[1:(length(dates)-6)]
# Check dimensions
dim(dataUS)
# now the dataset is in the correct form

# Convert to timeSeries
Time <- timeLastDayInMonth(dates)

#check how many NAs I have in each fund-column
colSums(is.na(dataUS))

# Convert to timeSeries with aligned dates
Time <- timeLastDayInMonth(dates)

Data <- timeSeries(dataUS, Time)
# convert the factor data into a dataframe
# exclude the first column which is the dates
factors<- as.data.frame(factors[,-1])
dim(factors)
head(factors)
#---------------------------------- PART A ----------------------------------------------

#-------------------------------------- A and a.------------------------------------------------
  
#-------------------- SINGLE FACTOR MODEL -----------------------------------------------
One_Factor_Data <- subset(factors, select = "Mkt-RF")
#673 1 because we have only 1 factor the 
dim(One_Factor_Data)

single_factor_data <- timeSeries(One_Factor_Data, Time)
dim(single_factor_data)

#-----------------Define In-sample period, Out-of-sample period, and Top Performing Funds--------

T <- dim(Data)[1]
k <- dim(Data)[2]
outofsampleperiod <- 48 # August 2015 to July 2019, Portfolio Construction (out of sample period)
insample <- T - outofsampleperiod  # Estimation (in sample period) 
TopProp <- 0.2  # top 20% selected funds, proportion of top funds
PlithosTop <- round(TopProp * k)
#here are 18 because 0.2*90 = 18
# number of top performing funds

#==========================================================================
#  Define In-sample returns and out-of-sample returns and factors
#==========================================================================
#==========================================================================
Ret=Data[1:insample,]
dim(Ret)
Ret_outofsample=Data[(insample+1):T,]
dim(Ret_outofsample)
Fact=single_factor_data[1:insample,]
dim(Fact)

#==========================================================================

#==========================================================================
#==========================     SHARPE RATIO   ============================
#==========================================================================
# Find the top performing fund based on SHARPE RATIO
# Invest in these top funds the next out-of-sample period
# There is not any rearangement in the portfolio
# The top funds could change every period

m_vec <- apply(Ret, 2, mean,na.rm = TRUE)
var_vec <- apply(Ret, 2, var,na.rm = TRUE)
std_vec <- sqrt(var_vec)
Sharpe_Ratio=m_vec/std_vec
SRsorted = sort(Sharpe_Ratio,index.return = TRUE)
SRsorted
#to take the returns of the sorted funds
Sharpe_Ratio_Sorted = SRsorted$x
Sharpe_Ratio_Sorted

#to take the sorted funds which are they
I_Sharpe_Ratio_Sorted = SRsorted$ix
I_Sharpe_Ratio_Sorted 

#take only the top 18 funds
I_Sharpe_Ratio_Use = I_Sharpe_Ratio_Sorted[(k-PlithosTop+1):k]
I_Sharpe_Ratio_Use

# Construct equally weighted portfolios based on SHARPE RATIO
# Ret_outofsample=DataRet[(insample+1):T,]
returns_Sharpe_Ratio=Ret_outofsample[,I_Sharpe_Ratio_Use]
dim(returns_Sharpe_Ratio)
MR_Sharpe = apply(returns_Sharpe_Ratio,1,mean)
#the cumulative return
CR_Sharpe=cumsum(MR_Sharpe)
plot(CR_Sharpe, type="l")

#==========================================================================
#==========================    TREYNOR RATIO   ============================
#==========================================================================
# Compute security betas (different for each fund)
betas<-NULL
k
for (i in 1:k) 
{
  y=Ret[,i] 
  x=Fact[,1]  
  yres <- lm(y ~ x)
  beta <- coef(yres)[2]
  betas <- cbind(betas,beta)
}
betas
# Find the top performing fund based on TREYNOR RATIO
m_vec = apply(Ret,2,mean,na.rm = TRUE)
Treynor_Ratio=m_vec/betas

TRsorted = sort(Treynor_Ratio,index.return = TRUE)
Treynor_Ratio_Sorted = TRsorted$x
I_Treynor_Ratio_Sorted = TRsorted$ix
I_Treynor_Ratio_Use = I_Treynor_Ratio_Sorted[(k-PlithosTop+1):k]
I_Treynor_Ratio_Use

# Construct equally weighted portfolios based on TREYNOR RATIO
returns_Treynor_Ratio = Ret_outofsample[,I_Treynor_Ratio_Use]
dim(returns_Treynor_Ratio)
MR_Treynor = apply(returns_Treynor_Ratio,1,mean)
CR_Treynor=cumsum(MR_Treynor)
plot(CR_Treynor, type="l")

#==========================================================================
#===========================    JENSEN ALPHA   ============================
#==========================================================================
# Compute alphas from the Single index model (different for each fund)
alphas<-NULL
for (i in 1:k) 
{
  y=Ret[,i]  
  x=Fact[,1]
  yres <- lm(y ~ x)
  alpha <- coef(yres)[1]
  alphas <- cbind(alphas,alpha)
}
alphas
# Find the top performing fund based on JENSEN ALPHA
JAsorted = sort(alphas,index.return = TRUE)
Jensen_alpha_Sorted = JAsorted$x
I_Jensen_alpha_Sorted = JAsorted$ix
I_Jensen_alpha_Use = I_Jensen_alpha_Sorted[(k-PlithosTop+1):k]
I_Jensen_alpha_Use

# Construct equally weighted portfolios based on JENSEN ALPHA
returns_Jensen_alpha = Ret_outofsample[,I_Jensen_alpha_Use]
dim(returns_Jensen_alpha)
MR_Jensen = apply(returns_Jensen_alpha,1,mean)
CR_Jensen = cumsum(MR_Jensen)
plot(CR_Jensen, type="l")
#==========================================================================
#==========================    SORTINO RATIO   ============================
#==========================================================================
# Find the top performing fund based on SORTINO RATIO
deltas <- NULL
for (i in 1:ncol(Ret)) {
  y <- na.omit(Ret[, i])
  mvalue <- mean(y, na.rm = TRUE)
  minvec <- pmin(0, y - mvalue)
  delta <- sqrt(sum(minvec^2) / length(y))
  deltas <- cbind(deltas, delta)
}
deltas 

y
m_vec = apply(Ret,2,mean,na.rm=TRUE)
Sortino_Ratio = m_vec/deltas
SOsorted = sort(Sortino_Ratio,index.return = TRUE)
SO_Sortino_Ratio_Sorted = SOsorted$x
I_Sortino_Ratio_Sorted = SOsorted$ix
I_Sortino_Ratio_Use = I_Sortino_Ratio_Sorted[(k-PlithosTop+1):k]
I_Sortino_Ratio_Use 
# Construct equally weighted portfolios based on SORTINO RATIO
returns_Sortino_Ratio = Ret_outofsample[,I_Sortino_Ratio_Use]
dim(returns_Sortino_Ratio)
MR_Sortino = apply(returns_Sortino_Ratio,1,mean)
CR_Sortino = cumsum(MR_Sortino)
plot(CR_Sortino, type="l")
#==========================================================================
#==========================================================================
#Top performing funds
Topfunds <- data.frame(round(cbind(I_Sharpe_Ratio_Use, I_Treynor_Ratio_Use, I_Jensen_alpha_Use, I_Sortino_Ratio_Use), 2))
names(Topfunds) <- c("Sharpe", "Treynor", "Jensen", "Sortino")
Topfunds

Results= c(mean(MR_Sharpe),  sd(MR_Sharpe),  CR_Sharpe[outofsampleperiod],  mean(MR_Sharpe)/sd(MR_Sharpe), 
           mean(MR_Treynor),  sd(MR_Treynor),  CR_Treynor[outofsampleperiod], mean(MR_Treynor)/sd(MR_Treynor),
           mean(MR_Jensen),   sd(MR_Jensen),   CR_Jensen[outofsampleperiod],  mean(MR_Jensen)/sd(MR_Jensen),
           mean(MR_Sortino),  sd(MR_Sortino),  CR_Sortino[outofsampleperiod], mean(MR_Sortino)/sd(MR_Sortino)) 

Res <- matrix(Results, nrow = 4, ncol=4, byrow=TRUE)
Res
#Cumulative Returns
plot(CR_Sharpe, main="", type="l", col="blue", ylab=NA, ylim=c(0,0.60), lwd=2, cex.axis=1.5, cex.lab=1.8)
lines(CR_Treynor, type="l", col="yellow", lwd=2)
lines(CR_Jensen, type="l", col="green", lwd=2)
lines(CR_Sortino, type="l", col="red", lwd=2)
legend("topleft", legend=c("Sharpe", "Treynor","Jensen","Sortino"), lty=c(1,1),
      pch=c(NA, 12), col=c("blue","yellow","green","red"), cex=1.2)
# Customizing the x-axis to show all dates
axis.Date(1, at = seq(min(Time[(insample + 1):T]), max(Time[(insample + 1):T]), by = "months"), 
          format = "%Y-%m", cex.axis = 1.5)
#--------------------------- b.---------------------------------------------------------

#----------------------- MULTIPLE REGRESSION MODELS ------------------------------------

factorsRet <- timeSeries(factors, Time)
Fact=factorsRet[1:insample,]
dim(Fact)
dim(factorsRet)
head(factorsRet)
T <- nrow(Data)  # total number of time periods
k <- ncol(Data)  # number of funds
Fact_outofsample <- factorsRet[(insample + 1):T,]

Y <- as.matrix(Ret)
X <- as.matrix(Fact)
alphas <- NULL
# Loop through each fund and fit the regression model using stepwise selection
for (i in 1:ncol(Y)) {
  y <- Y[, i]
  x <- X
  data <- data.frame(y, x)
  # Full model with all variables
  full_model <- lm(y ~ ., data = data)
  # stepwise selection
  stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)
  # Extract alpha from the selected model
  alpha <- coef(stepwise_model)[1]
  alphas <- cbind(alphas, alpha)
}

# Find the top performing funds based on JENSEN ALPHA
JAsorted <- sort(alphas, index.return = TRUE)
Jensen_alpha_Sorted <- JAsorted$x
I_Jensen_alpha_Sorted <- JAsorted$ix
I_Jensen_alpha_Use <- I_Jensen_alpha_Sorted[(k - PlithosTop + 1):k]
I_Jensen_alpha_Use

# Construct equally weighted portfolios based on Jensen Alpha
returns_Jensen_alpha <- Ret_outofsample[, I_Jensen_alpha_Use]
dim(returns_Jensen_alpha)
MR_Jensen_multiple <- apply(returns_Jensen_alpha, 1, mean)
CR_Jensen_multiple <- cumsum(MR_Jensen_multiple)
# Plot Cumulative Returns
plot(CR_Jensen_multiple, type = "l", main = "", col = "seagreen", lwd = 2, 
     xlab = "Time", ylab = "Cumulative Return", cex.axis = 1.8, cex.lab = 1.8,xaxt = "n")


# Customizing the x-axis to show all dates
axis.Date(1, at = seq(min(Time[(insample + 1):T]), max(Time[(insample + 1):T]), by = "months"), 
          format = "%Y-%m", cex.axis = 1.5)


alphas
#----------------------------------------- c. -----------------------------------------------------

#---------------------------------- MULTIPLE REGRESSION – GARCH TYPE MODELS -----------------------------
Y <- as.matrix(Ret)
X <- as.matrix(Fact)

results <- list()
#Loop through each fund and perform the ACF tests ,normality tests,histograms,QQ plots
# ACF and PACF plots
for (i in 1:ncol(Y)) {
  y <- Y[, i]
  y1 <- na.omit(y)
  
  fund_results <- list()
  # Histogramm of the returns of the fund
  hist(y1, main = paste("Histogram of returns for Fund", i))
  # QQ norm in order to see if there is normality or not in the returns
  qqnorm(y1, main = paste("Normal QQplot of Fund", i)) 
  qqline(y1)
  # ACF plot of the returns of the fund
  acf(y1, 50, main = paste("ACF of returns for Fund", i))
  # PACF plot of the returns of the fund
  pacf(y1, 50, main = paste("PACF of returns for Fund", i))
  # ACF plot of the squared residuals of each fund
  acf(y1^2, 50, main = paste("ACF of squared returns for Fund", i))
  # PACF plot of the squared residuals of each fund
  pacf(y1^2, 50, main = paste("PACF of squared returns for Fund", i))
  
  fund_results$box_test <- Box.test(y1, lag = 12, type = "Ljung") 
  fund_results$box_test_squared <- Box.test(y1^2, lag = 12, type = "Ljung") 

  # Normality Test
  fund_results$jarque_bera <- jarque.bera.test(y1)
  fund_results$shapiro <- shapiro.test(y1)
  
  results[[paste("Fund", i)]] <- fund_results
}
# Print the results for the last fund 
print(results[["Fund 90"]])

y <- Y[, 1]
y1 <- na.omit(y)

# Initialize a list to store the results for Fund 90
fund_1_results <- list()

par(mfrow = c(3, 2), mar = c(4, 4, 4, 2) + 0.1, cex.lab = 1.5, cex.axis = 1.6, cex.main = 1.8)

# Histogram of the returns of Fund 90
hist(y1, main = "Histogram of returns", xlab = "Returns", ylab = "Frequency", col = "lightblue")

# QQ plot to check for normality
qqnorm(y1, main = "Normal QQplot", ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")
qqline(y1, col = "red")

# ACF plot of the returns of Fund 90
acf_result <- acf(y1, 30, plot = FALSE)
plot(acf_result, main = "ACF of residuals", ylab = "ACF", xlab = "Lag", lwd = 2)
abline(h = c(-1.96 / sqrt(length(y1)), 1.96 / sqrt(length(y1))), col = "blue", lwd = 2, lty = 2)

# PACF plot of the returns of Fund 90
pacf_result <- pacf(y1, 30, plot = FALSE)
plot(pacf_result, main = "PACF of residuals", ylab = "PACF", xlab = "Lag", lwd = 2)
abline(h = c(-1.96 / sqrt(length(y1)), 1.96 / sqrt(length(y1))), col = "blue", lwd = 2, lty = 2)

# ACF plot of the squared returns of Fund 90
acf_result_squared <- acf(y1^2, 30, plot = FALSE)
plot(acf_result_squared, main = "ACF of squared residuals", ylab = "ACF", xlab = "Lag", lwd = 2)
abline(h = c(-1.96 / sqrt(length(y1^2)), 1.96 / sqrt(length(y1^2))), col = "blue", lwd = 2, lty = 2)

# PACF plot of the squared returns of Fund 90
pacf_result_squared <- pacf(y1^2, 30, plot = FALSE)
plot(pacf_result_squared, main = "PACF of squared residuals", ylab = "PACF", xlab = "Lag", lwd = 2)
abline(h = c(-1.96 / sqrt(length(y1^2)), 1.96 / sqrt(length(y1^2))), col = "blue", lwd = 2, lty = 2)
# Box-Ljung test for autocorrelation in the returns
fund_1_results$box_test <- Box.test(y1, lag = 30, type = "Ljung")

# Box-Ljung test for autocorrelation in the squared returns
fund_1_results$box_test_squared <- Box.test(y1^2, lag = 30, type = "Ljung")

# Normality tests
fund_1_results$jarque_bera <- jarque.bera.test(y1)
fund_1_results$shapiro <- shapiro.test(y1)

# Print the results
print(fund_1_results)

# for example for the fund 90 we see that there is significant correlation
# in the returns and in the squared residuals as p value is significantly larger
# than 0.05 so we do not reject the Ho that ρ1=ρ2=...=ρ12=0
# also from the results of Shapiro and Jarque bera test we reject Ho
# so we do not have normality


###--------------------------- FIT GARCH (1,1) MODEL --------------------------------------



alphas_g1_1 <- NULL

for (i in 1:ncol(Y)) {
  y <- Y[, i]
  y <- na.omit(y)
  
  # Combined Multiple Regression + GARCH Model
  #Specify the GARCH model with external regressors (which correspond to
  # the explanatory variables X1,t....Xk,t

  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)), 
                     mean.model = list(armaOrder=c(0,0), include.mean = TRUE, external.regressors = X[1:length(y),]),
                     distribution.model = "norm")
  #Fit the model
  modelres <- ugarchfit(spec = spec, data = y)
  print(modelres)
  
  # Extract alpha (intercept) from the model
  alpha <- coef(modelres)["mu"]
  alphas_g1_1 <- cbind(alphas_g1_1, alpha)
}
# Find the top performing funds based on JENSEN ALPHA
# when g1_1 we mean the GARCH(1,1) model

alphas_sorted_g1_1 <- sort(alphas_g1_1, index.return = TRUE)
JA_sorted_g1_1 <- alphas_sorted_g1_1$x
I_JA_sorted_g1_1 <- alphas_sorted_g1_1$ix
I_JA_Use_g1_1 <- I_JA_sorted_g1_1[(k - PlithosTop + 1):k]
I_JA_Use_g1_1 

par(mfrow = c(1, 1))
# Construct equally weighted portfolios based on JA
returns_JA_g1_1 <- Ret_outofsample[, I_JA_Use_g1_1]
dim(returns_JA_g1_1)
MR_Jensen_g1_1 <- apply(returns_JA_g1_1, 1, mean)
CR_Jensen_g1_1 <- cumsum(MR_Jensen_g1_1)


plot(CR_Jensen_g1_1, type = "l", col = "blue4", lwd = 2, 
     cex.axis = 1.5, cex.lab = 1.5, xlab = "Time", ylab = "Cumulative Return", 
     main = "", xaxt = "n")

# Customizing the x-axis to show all dates
axis.Date(1, at = seq(min(Time[(insample + 1):T]), max(Time[(insample + 1):T]), by = "months"), 
          format = "%Y-%m", cex.axis = 1.5)

alphas_g1_1

#-------------------------------------- d. -----------------------------------------------

#---------------------------- ALTERNATIVE MODELLING APPROACH ------------------------------


#------------------- FIT A MULTIPLE REGRESSION TYPE - GARCH (2,2) -------------------------

#with the code g2_2 we mean the GARCH(2,2) model
alphas_g2_2 <- NULL
for (i in 1:ncol(Y)) {
  y <- Y[, i]
  y <- na.omit(y)
  
  # Combined Multiple Regression + GARCH(2,2) Model
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), 
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE, external.regressors = X[1:length(y),]),
                     distribution.model = "norm")
  modelres <- ugarchfit(spec = spec, data = y)
  print(modelres)
  
  # Extract alpha (intercept) from the combined model
  alpha <- coef(modelres)["mu"]
  alphas_g2_2 <- cbind(alphas_g2_2, alpha)
}
# Find the top performing funds based on JENSEN ALPHA
alphas_sorted_g2_2  <- sort(alphas_g2_2, index.return = TRUE)
JA_sorted_g2_2 <- alphas_sorted_g2_2$x
I_JA_sorted_g2_2  <- alphas_sorted_g2_2$ix
I_JA_Use_g2_2  <- I_JA_sorted_g2_2[(k - PlithosTop + 1):k]
I_JA_Use_g2_2 

# Construct equally weighted portfolios based on Jensen Alpha
returns_JA_g2_2  <- Ret_outofsample[, I_JA_Use_g2_2 ]
MR_Jensen_g2_2 <- apply(returns_JA_g2_2,1,mean)
CR_Jensen_g2_2 <- cumsum(MR_Jensen_g2_2)
plot(CR_Jensen_g2_2, type = "l", main = "", col = "red4",lwd = 2, cex.axis = 1.5, cex.lab = 1.5,
     xlab = "Time", ylab = "Cumulative Return")
# Customizing the x-axis to show all dates
axis.Date(1, at = seq(min(Time[(insample + 1):T]), max(Time[(insample + 1):T]), by = "months"), 
          format = "%Y-%m", cex.axis = 1.5)
alphas_g2_2
#-------------------- FIT EGARCH (1,1) ------------------------------------------
fit_egarch_model <- function(y, x) {
  spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), 
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE, external.regressors = x),
                     distribution.model = "norm")
  modelres <- ugarchfit(spec = spec, data = y)
  return(modelres)
}
# Loop through each fund 
alphas_egarch <- NULL
aic_egarch <- NULL
bic_egarch <- NULL
loglik_egarch <- NULL
for (i in 1:ncol(Y)) {
  y <- Y[, i]
  y <- na.omit(y)
  x <- X[1:length(y),]
  
  # Fit EGARCH(1,1) Model
  modelres_egarch <- fit_egarch_model(y, x)
  alpha_egarch <- coef(modelres_egarch)["mu"]
  alphas_egarch <- cbind(alphas_egarch, alpha_egarch)
  aic_egarch <- c(aic_egarch, infocriteria(modelres_egarch)["AIC"])
  bic_egarch <- c(bic_egarch, infocriteria(modelres_egarch)["BIC"])
  loglik_egarch <- c(loglik_egarch, likelihood(modelres_egarch))
}
# Find the top performing funds based on JENSEN ALPHA
JAsorted_egarch <- sort(alphas_egarch, index.return = TRUE)
Jensen_alpha_Sorted_egarch <- JAsorted_egarch$x
I_Jensen_alpha_Sorted_egarch <- JAsorted_egarch$ix
I_Jensen_alpha_Use_egarch <- I_Jensen_alpha_Sorted_egarch[(k - PlithosTop + 1):k]
I_Jensen_alpha_Use_egarch 
# Construct equally weighted portfolios based on Jensen Alpha
returns_Jensen_alpha_egarch <- Ret_outofsample[, I_Jensen_alpha_Use_egarch]
MR_Jensen_egarch <- apply(returns_Jensen_alpha_egarch, 1, mean)
CR_Jensen_egarch <- cumsum(MR_Jensen_egarch)
plot(CR_Jensen_egarch, type = "l", main = "", 
col = "magenta4", lwd = 2, cex.axis = 1.5, cex.lab = 1.5,
xlab = "Time", ylab = "Cumulative Return")
# Customizing the x-axis to show all dates
axis.Date(1, at = seq(min(Time[(insample + 1):T]), max(Time[(insample + 1):T]), by = "months"), 
          format = "%Y-%m", cex.axis = 1.5)
alphas_egarch


#---------------------- Comparison of the different models-----------------------------------

# Plot the cumulative returns from different models on the same graph
plot(CR_Jensen_g1_1, type = "l", col = "steelblue1", lwd = 5, xlab = "Time", ylab = "Cumulative Return",
     main = "", xaxt = "n", cex.axis = 1.8, cex.lab = 1.8)
lines(CR_Jensen_g2_2, type = "l", col = "tomato3", lwd =5)
lines(CR_Jensen_egarch, type = "l", col = "magenta4", lwd =5)
lines(CR_Jensen_multiple, type = "l", col = "seagreen", lwd = 5)
lines(CR_Jensen,type="l",col="yellow3",lwd=5)

# Customizing the x-axis to show all dates
axis.Date(1, at = seq(min(Time[(insample + 1):T]), max(Time[(insample + 1):T]), by = "months"), 
          format = "%Y-%m", cex.axis = 1.8)
legend("topleft", legend = c("GARCH(1,1)", "GARCH(2,2)", "EGARCH(1,1)", "Multiple Regression", "Single Index"), 
       col = c("steelblue1", "tomato3", "magenta4", "seagreen", "yellow3"), lty = 1, lwd = 4, cex = 1.2, inset = c(0.01, 0.01),
       box.lwd = 0, box.col = "black", bg = "white", pt.cex = 0.1, text.width = strwidth("Multiple Regression", cex = 1.1),
       adj = c(0.1, 0.1))
#---------------Comparison of Jensen Alpha top 18 funds---------------------------------

funds <- paste("Fund", 1:18, sep = "")
alphas_single <- as.numeric(alphas)
alphas_multiple <- as.numeric(alphas)
alphas_garch_1_1 <- as.numeric(alphas_g1_1)
alphas_garch_2_2 <- as.numeric(alphas_g2_2)
alphas_egarch <- as.numeric(alphas_egarch)

# Ensure that the lengths match, if not, adjust accordingly
alphas_single <- alphas_single[1:18]
alphas_multiple <- alphas_multiple[1:18]
alphas_garch_1_1 <- alphas_garch_1_1[1:18]
alphas_garch_2_2 <- alphas_garch_2_2[1:18]
alphas_egarch <- alphas_egarch[1:18]

# Create a data frame
df <- data.frame(
  Fund = rep(funds, times = 5),
  Model = rep(c("Single Factor", "Multiple Regression", "GARCH(1,1)", "GARCH(2,2)", "EGARCH"), each = 18),
  Alpha = c(alphas_single, alphas_multiple, alphas_garch_1_1, alphas_garch_2_2, alphas_egarch)
)

# Ensure the Fund column is treated as a factor for correct ordering in the plot
df$Fund <- factor(df$Fund, levels = funds)

# Plot using ggplot2
ggplot(df, aes(x = Fund, y = Alpha, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge",width = 1.3) +
  theme_minimal() +
  labs(title = "",
       x = "Fund", y = "Jensen's Alpha") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16), 
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 20, hjust = 0.8),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)) +
  scale_fill_manual(values = c("Single Factor" = "yellow3", 
  "Multiple Regression" = "seagreen",
  "GARCH(1,1)" = "steelblue1", "GARCH(2,2)" = "tomato3", "EGARCH" = "magenta4"))
#---------------------------------- PART Β ----------------------------------------------

# ------------------------------------ a. ---------------------------------------------
# The mean vector and the covariance matrix of returns 
# is estimated using the Sample estimate of mean vector and covariance matrix.


# the target return will be on a monthly basis and will be 0.01
targetreturn <- 0.01

# Custom covariance function in order to handle the NA values
custom_cov=function(data){
  nvar=ncol(data)
  cov_matrix=diag(nvar)
  for (i in 1:nvar){
    var1=data[,i]
    var1=var1[is.na(var1)==FALSE]
    for(j in 1:nvar){
      if(i!=j){
        var2=data[,j]
        var2=var2[is.na(var2)==FALSE]
        l1=length(var1)
        l2=length(var2)
        if(l1==l2){
          cov_matrix[i,j]=cov_matrix[j,i]=cov(var1,var2)
        }else if(l1>l2){
          var1=var1[(l1-l2+1):l1]
          cov_matrix[i,j]=cov_matrix[j,i]=cov(var1,var2)
        }else{#l1<l2
          var2=var2[(l2-l1+1):l2]
          cov_matrix[i,j]=cov_matrix[j,i]=cov(var1,var2)
        }
      }
    }
  }
  return(cov_matrix)
}
#==========================================================================
#Predict Future Covariances using sample MEANS and COVARIANCES
#==========================================================================
# Extract in-sample data
DataRetIS <- Data[1:insample, ]

dim(DataRetIS)

m_vec <- apply(DataRetIS, 2, function(x) mean(x, na.rm=TRUE))
max(m_vec)
cov_mat <- custom_cov(DataRetIS)
#===========================================================================================================
# CONSTRUCTION OF MEAN VARIANCE PORTFOLIO BASED ON THE SAMPLE ESTIMATE OF MEAN VECTOR AND COV MATRIX 
#===========================================================================================================
# set matrices with constraints
D.mat <-  2*cov_mat
d.vec <-  rep(0, k)
A.mat <-  cbind(rep(1,k), m_vec, diag(k))
b.vec <-  c(1, targetreturn, rep(0,k))

is.na(m_vec)
is.na(cov_mat)
max(m_vec)
# Solve the Quadratic Programming Problem
qp.Result <-  solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
w_meanvar <-  as.matrix(round(qp.Result$solution,5), k, 1)
cbind((1:k), w_meanvar)

# w are the optimal portfolio weights (k x 1 vector)
# Estimated portfolio expected returns
PortReturn_meanvar <-  m_vec%*%w_meanvar 
# Estimated portfolio Risk (standard deviation)
PortRisk_meanvar <-   sqrt(t(w_meanvar)%*%cov_mat%*%w_meanvar)  

# ----------------------------------- OUT OF SAMPLE ----------------------------------------------
# 
# Calculate out-of-sample covariance matrix
cov_mat_outofsample <- custom_cov(Ret_outofsample)

dim(Ret_outofsample)
dim(cov_mat_outofsample)

#calculate Out of Sample Returns
RR_meanvar <-  Ret_outofsample %*% w_meanvar

#calculate Portfolio Risk (portfolio standard deviation)
PR_meanvar <- sqrt(t(w_meanvar) %*% cov_mat_outofsample %*% w_meanvar)

# Realized return of mean var
RealizedReturn_meanvar <- mean(RR_meanvar)

# Cumulative return of mean var
Cum_Ret_meanvar <- cumsum(RR_meanvar)

#calculate Conditional Sharp Ratio (CSR)
CSR_meanvar <- mean(RR_meanvar) / PR_meanvar

round(cbind(PortReturn_meanvar, PortRisk_meanvar, 
                               mean(RR_meanvar), PR_meanvar, CSR_meanvar), 4)

#===========================================================================================================
# CONSTRUCTION OF MIN VARIANCE PORTFOLIO BASED ON THE SAMPLE ESTIMATE OF MEAN VECTOR AND COV MATRIX 
#===========================================================================================================
# set matrices with constraints
# Dmat is the matrix appearing in the quadratic function to be minimized
D.mat <- 2*cov_mat

d.vec <- rep(0, k)
# Amat	is a matrix defining the constraints under which we
# want to minimize the quadratic function.

A.mat <- cbind(rep(1,k), diag(k))
# bvec	is a vector holding the values of b_0 (defaults to zero)

b.vec <- c(1, rep(0,k))

# meq the first meq constraints are treated as
# equality constraints, all further as inequality constraints (defaults to 0)

# Solve the Quadratic Programming Problem
qp.Result <- solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
w_minvar <- as.matrix(round(qp.Result$solution,5), k, 1)
cbind((1:k), w_minvar)

# w are the optimal portfolio weights (k x 1 vector)
# Estimated portfolio expected returns
PortReturn_minvar <- m_vec%*%w_minvar  
# Estimated portfolio Risk (standard deviation)
PortRisk_minvar <-  sqrt(t(w_minvar)%*%cov_mat%*%w_minvar)  

# ----------------------------------- OUT OF SAMPLE ----------------------------------------------
# 
RR_minvar<- Ret_outofsample%*%w_minvar

# Cumulative return of min var
Cum_Ret_minvar <- cumsum(RR_minvar)

#realized return of min var
RealizedReturn_minvar <- mean(RR_minvar)

#calculate Portfolio Risk (portfolio standard deviation)
PR_minvar<-sqrt(t(w_minvar)%*%cov_mat_outofsample%*%w_minvar)

#calculate Conditional Sharp Ratio (CSR)
CSR_minvar <- mean(RR_minvar) / PR_minvar

round(cbind(PortReturn_minvar, PortRisk_minvar, mean(RR_minvar), PR_minvar, CSR_minvar), 4)

# Evaluate the constructed portfolios based on (i) the realized returns, (ii) the cumulative 
# returns, and (iii) the Conditional Sharpe Ratio, for the out-of-sample period 8/2015-7/2019.
evaluations <- list(
  MeanVariance = list(
    PortReturn = round(PortReturn_meanvar, 4),
    PortRisk = round(PortRisk_meanvar, 4),
    RealizedReturn = round(RealizedReturn_meanvar, 4),
    ConditionalSharpeRatio = round(CSR_meanvar, 4)
  ),
  MinVariance = list(
    PortReturn = round(PortReturn_minvar, 4),
    PortRisk = round(PortRisk_minvar, 4),
    RealizedReturn = round(RealizedReturn_minvar, 4),
    ConditionalSharpeRatio = round(CSR_minvar, 4)
  )
)
print(evaluations)
#==========================================================================
# PLOT CUMULATIVE RETURNS
#==========================================================================
Cum_Ret_meanvar <- cumsum(RR_meanvar)
Cum_Ret_minvar <- cumsum(RR_minvar)

dates <- seq(as.Date("2015-08-01"), by = "month", length.out = length(Cum_Ret_meanvar))

# Cumulative Portfolio Returns
plot(dates, Cum_Ret_meanvar, type="l", col="blue", lwd=3, xlab="Time", ylab="Cumulative Return", 
     main="", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, ylim=c(-0.2, 0.4))
lines(dates, Cum_Ret_minvar, type="l", col="red", lwd=3)
legend("topleft", legend=c("Mean-Variance Portfolio", "Minimum Variance Portfolio"), 
       col=c("blue", "red"), lty=1, lwd=3, cex=1.4, inset=c(0.01, 0.01))

# Customize the x-axis to show the actual dates by year without duplication
axis.Date(1, at = seq(as.Date("2016-01-01"), as.Date("2019-07-01"), by = "years"), format = "%Y", cex.axis = 1.5)
#----------------------------- b. ---------------------------------------------
# The mean vector and the covariance matrix of returns are estimated
# Based on the single index model of the form

#----------------------------USING SINGLE INDEX MODEL -------------------------------------------
Ret=Data[1:insample,]
dim(Ret)
Ret_outofsample=Data[(insample+1):T,]
dim(Ret_outofsample)
Fact=single_factor_data[1:insample,]
dim(Fact)
# Extract the single factor data for the out-of-sample period
Fact_outofsample <- single_factor_data[(insample+1):T, ]
dim(Fact_outofsample)
#==========================================================================
# CONSTRUCTION OF MEAN-VARIANCE PORTFOLIOS BASED ON THE SINGLE INDEX MODEL
#==========================================================================
alphas <- rep(NA, k)
betas <- rep(NA, k)
residual_vars <- rep(NA, k)

for (i in 1:k) {
  y=Ret[,i] 
  x=Fact[,1]
  yres <- lm(y ~ x)
  alphas[i] <- coef(yres)[1]
  betas[i] <- coef(yres)[2]
  residual_vars[i] <- var(residuals(yres))
}
# Estimate mean return of the market factor
mean_market_return <- mean(Fact)

# Estimate mean returns of funds
m_vec_sim <- alphas + betas * mean_market_return

max(m_vec_sim)

# Estimate covariance matrix using the single index model
cov_mat_sim <- matrix(0, ncol = k, nrow = k)

for (i in 1:k) {
  for (j in 1:k) {
    if (i == j) {
      cov_mat_sim[i, j] <- betas[i]^2 * var(Fact) + residual_vars[i]
    } else {
      cov_mat_sim[i, j] <- betas[i] * betas[j] * var(Fact)
    }
  }
}

dim(cov_mat_sim)
# The target return will be on a monthly basis and will be 0.07
targetreturn <- 0.009
# Construct Mean-Variance Portfolio
D.mat <- 2 * cov_mat_sim
d.vec <- rep(0, k)
A.mat <- cbind(rep(1, k), m_vec_sim, diag(k))
b.vec <- c(1, targetreturn, rep(0, k))

# Solve the Quadratic Programming Problem
qp.Result_sim_meanvar <- solve.QP(Dmat = D.mat, dvec = d.vec, Amat = A.mat, bvec = b.vec, meq = 1)
w_meanvar_sim <- as.matrix(round(qp.Result_sim_meanvar$solution, 5), k, 1)
cbind((1:k), w_meanvar_sim)

# Calculate in-sample performance

# portfolio return in sample
PortReturn_meanvar_sim <- m_vec_sim %*% w_meanvar_sim
# portfolio in sample risk
PortRisk_meanvar_sim <- sqrt(t(w_meanvar_sim) %*% cov_mat_sim %*% w_meanvar_sim)

# ----------------------------------- OUT OF SAMPLE ----------------------------------------------
# 
# Calculate out-of-sample performance
mean_market_return_outofsample <- mean(Fact_outofsample)
m_vec_sim_outofsample <- mean_market_return_outofsample * betas

# Estimate out-of-sample covariance matrix using single index model
cov_mat_sim_outofsample <- matrix(0, ncol = k, nrow = k)

for (i in 1:k) {
  for (j in 1:k) {
    if (i == j) {
      cov_mat_sim_outofsample[i, j] <- betas[i]^2 * var(Fact_outofsample) + residual_vars[i]
    } else {
      cov_mat_sim_outofsample[i, j] <- betas[i] * betas[j] * var(Fact_outofsample)
    }
  }
}

# realized return of mean-variance portfolio
RR_meanvar_sim_outofsample <- Ret_outofsample %*% w_meanvar_sim
mean(RR_meanvar_sim_outofsample )
#calculate Portfolio Risk (portfolio standard deviation)
PR_meanvar_sim_outofsample <- sqrt(t(w_meanvar_sim) %*% cov_mat_sim_outofsample %*% w_meanvar_sim)

# Condiotal Sharpe Ratio 
CSR_meanvar_sim_outofsample <- mean(RR_meanvar_sim_outofsample) / PR_meanvar_sim_outofsample


round(cbind(PortReturn_meanvar_sim, PortRisk_meanvar_sim,
            mean(RR_meanvar_sim_outofsample), PR_meanvar_sim_outofsample,
            CSR_meanvar_sim_outofsample), 4)
#==========================================================================
# CONSTRUCTION OF MIN-VARIANCE PORTFOLIOS BASED ON THE SINGLE INDEX MODEL
#==========================================================================
# Construct Minimum Variance Portfolio
D.mat_minvar <- 2 * cov_mat_sim
d.vec_minvar <- rep(0, k)
A.mat_minvar <- cbind(rep(1, k), diag(k))
b.vec_minvar <- c(1, rep(0, k))

# Solve the Quadratic Programming Problem
qp.Result_sim_minvar <- solve.QP(Dmat = D.mat_minvar, dvec = d.vec_minvar, Amat = A.mat_minvar, bvec = b.vec_minvar, meq = 1)
w_minvar_sim <- as.matrix(round(qp.Result_sim_minvar$solution, 5), k, 1)
cbind((1:k), w_minvar_sim)

# Calculate in-sample performance
PortReturn_minvar_sim <- m_vec_sim %*% w_minvar_sim
PortRisk_minvar_sim <- sqrt(t(w_minvar_sim) %*% cov_mat_sim %*% w_minvar_sim)

# ----------------------------------- OUT OF SAMPLE ----------------------------------------------
# 
# The out-of-sample mean return vector was calculated previously
mean_market_return_outofsample <- mean(Fact_outofsample)
m_vec_sim_outofsample <- mean_market_return_outofsample * betas

# Estimate out-of-sample covariance matrix using single index model
cov_mat_sim_outofsample <- matrix(0, ncol = k, nrow = k)

for (i in 1:k) {
  for (j in 1:k) {
    if (i == j) {
      cov_mat_sim_outofsample[i, j] <- betas[i]^2 * var(Fact_outofsample) + residual_vars[i]
    } else {
      cov_mat_sim_outofsample[i, j] <- betas[i] * betas[j] * var(Fact_outofsample)
    }
  }
}

# Realized return of min-variance portfolio
RR_minvar_sim_outofsample <- Ret_outofsample %*% w_minvar_sim
mean(RR_minvar_sim_outofsample)
#calculate Portfolio Risk (portfolio standard deviation)
PR_minvar_sim_outofsample <- sqrt(t(w_minvar_sim) %*% cov_mat_sim_outofsample %*% w_minvar_sim)

# Contidional Sharpe Ratio
CSR_minvar_sim_outofsample <- mean(RR_minvar_sim_outofsample) / PR_minvar_sim_outofsample

round(cbind(PortReturn_minvar_sim, PortRisk_minvar_sim,
            mean(RR_minvar_sim_outofsample), PR_minvar_sim_outofsample,
            CSR_minvar_sim_outofsample), 4)
#==========================================================================
# PLOT CUMULATIVE RETURNS
#==========================================================================
Cum_Ret_sim_meanvar <- cumsum(RR_meanvar_sim_outofsample)
Cum_Ret_sim_minvar <- cumsum(RR_minvar_sim_outofsample)


# Cumulative Portfolio Returns
plot(dates, Cum_Ret_sim_meanvar, type="l", col="blue", lwd=3, xlab="Time", ylab="Cumulative Return", 
     main="", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, ylim=c(-0.2, 0.6))
lines(dates, Cum_Ret_sim_minvar, type="l", col="red", lwd=3)
legend("topleft", legend=c("Mean-Variance Portfolio", "Minimum Variance Portfolio"), 
       col=c("blue", "red"), lty=1, lwd=3, cex=1.5, inset=c(0.01, 0.01))

# Customize the x-axis to show the actual dates by year without duplication
axis.Date(1, at = seq(as.Date("2016-01-01"), as.Date("2019-07-01"), by = "years"), format = "%Y", cex.axis = 1.5)


# Evaluate the constructed portfolios based on (i) the realized returns, (ii) the cumulative 
# returns, and (iii) the Conditional Sharpe Ratio, for the out-of-sample period 8/2015-7/2019.
# Evaluate the portfolios
sim_evaluations <- list(
  MeanVariance = list(
    PortReturn = round(PortReturn_meanvar_sim, 4),
    PortRisk = round(PortRisk_meanvar_sim, 4),
    RealizedReturn = round(mean(RR_meanvar_sim_outofsample), 4),
    ConditionalSharpeRatio = round(CSR_meanvar_sim_outofsample, 4)
  ),
  MinVariance = list(
    PortReturn = round(PortReturn_minvar_sim, 4),
    PortRisk = round(PortRisk_minvar_sim, 4),
    RealizedReturn = round(mean(RR_minvar_sim_outofsample), 4),
    ConditionalSharpeRatio = round(CSR_minvar_sim_outofsample, 4)
  )
)
print(sim_evaluations)
#------------------------------------ c. ---------------------------------------------------------

# The mean vector and the covariance matrix of returns are estimated
# Based on multivariate multiple regression models

factorsRet <- timeSeries(factors, Time)
Fact=factorsRet[1:insample,]
dim(Fact)
dim(factorsRet)
head(factorsRet)

T <- nrow(Data)  # total number of time periods
k <- ncol(Data)  # number o f funds

Fact_outofsample <- factorsRet[(insample + 1):T,]
dim(Fact_outofsample)

#==========================================================================================
# CONSTRUCTION OF MEAN-VARIANCE PORTFOLIOS BASED ON MULTIVARIATE MULTIPLE REGRESSION MODELS
#==========================================================================================
# Prepare the data
Y <- as.matrix(Ret)
X <- as.matrix(Fact)

# Initialize matrices and vectors
alphas <- numeric(k)
betas <- matrix(NA, nrow = k, ncol = ncol(X))
residual_vars <- matrix(0, nrow = k, ncol = k)
residuals_list <- list()


# Loop through each fund to fit the regression model
for (i in 1:k) {
  y <- Y[, i]
  data <- data.frame(y, X)
  data <- na.omit(data)
  
  # Regress only if there are enough data points
  if (nrow(data) > 1) {
    model <- lm(y ~ ., data = data)
    coefs <- coef(model)
    
    alphas[i] <- coefs[1]
    betas[i, ] <- coefs[-1]
    
    # Calculate residual variance
    residuals <- residuals(model)
    residual_vars[i, i] <- var(residuals)
    residuals_list[[i]] <- residuals
  }
}

# Convert alphas to column matrix
alphas <- matrix(alphas, ncol = 1)
dim(alphas)
dim(betas)
# Calculate mean returns of factors
mean_factors_return <- colMeans(X, na.rm = TRUE)
mean_factors_return <- matrix(mean_factors_return, ncol = 1)

# Estimate mean returns of funds
m_vec_multivar <- alphas + betas %*% mean_factors_return
print(dim(m_vec_multivar)) 
max(m_vec_multivar)
# Estimate covariance matrix using the multivariate regression model
factor_cov <- cov(X, use = "complete.obs")
cov_mat_multivar <- residual_vars + betas %*% factor_cov %*% t(betas)
dim(cov_mat_multivar)

# Check for positive definiteness
eigen_values <- eigen(cov_mat_multivar)$values
if (any(eigen_values <= 0)) {
  stop("Covariance matrix is not positive definite even after regularization!")
}

targetreturn <- 0.01
# Construct Mean-Variance Portfolio
D.mat <- 2 * cov_mat_multivar
d.vec <- rep(0, k)
A.mat <- cbind(rep(1, k), m_vec_multivar, diag(k))
b.vec <- c(1, targetreturn, rep(0, k))

# Solve the Quadratic Programming Problem
qp.Result_multivar_meanvar <- solve.QP(Dmat = D.mat, dvec = d.vec, Amat = A.mat, bvec = b.vec, meq = 1)

# Extract optimal weights
w_meanvar_multivar <- round(qp.Result_multivar_meanvar$solution, 5)

cbind((1:ncol(Y)), w_meanvar_multivar)

# Calculate in-sample performance
PortReturn_meanvar_multivar <- sum(m_vec_multivar * w_meanvar_multivar)
PortRisk_meanvar_multivar <- sqrt(t(w_meanvar_multivar) %*% cov_mat_multivar %*% w_meanvar_multivar)

print(round(cbind(PortReturn_meanvar_multivar, PortRisk_meanvar_multivar), 4))

#--------------------------- OUT OF SAMPLE -----------------------------------------------------

# Calculate out-of-sample performance
X_outofsample <- as.matrix(Fact_outofsample)
Y_outofsample <- as.matrix(Ret_outofsample)

# Calculate mean returns of factors out-of-sample
mean_factors_return_outofsample <- colMeans(X_outofsample, na.rm = TRUE)
mean_factors_return_outofsample <- matrix(mean_factors_return_outofsample, ncol = 1)

# Estimate mean returns of funds out-of-sample
m_vec_multivar_outofsample <- alphas + betas %*% mean_factors_return_outofsample

# Realized return of mean-variance portfolio out-of-sample
RR_meanvar_multivar_outofsample <- Y_outofsample %*% w_meanvar_multivar
mean(RR_meanvar_multivar_outofsample)
# Calculate portfolio risk (standard deviation) out-of-sample
cov_mat_multivar_outofsample <- residual_vars + betas %*% cov(X_outofsample) %*% t(betas)
PR_meanvar_multivar_outofsample <- sqrt(t(w_meanvar_multivar) %*% cov_mat_multivar_outofsample %*% w_meanvar_multivar)

# Conditional Sharpe Ratio out-of-sample
CSR_meanvar_multivar_outofsample <- mean(RR_meanvar_multivar_outofsample) / PR_meanvar_multivar_outofsample

# Calculate cumulative returns
Cum_Ret_multivar_meanvar <- cumsum(RR_meanvar_multivar_outofsample)

#==========================================================================================
# CONSTRUCTION OF MIN-VARIANCE PORTFOLIOS BASED ON MULTIVARIATE MULTIPLE REGRESSION MODELS
#==========================================================================================
# Initialize matrices and vectors
# Calculate mean returns of factors
mean_factors_return <- colMeans(X, na.rm = TRUE)
mean_factors_return <- matrix(mean_factors_return, ncol = 1)

# Estimate mean returns of funds
m_vec_multivar <- alphas + betas %*% mean_factors_return
print(dim(m_vec_multivar))
print(max(m_vec_multivar))

# Estimate covariance matrix using the multivariate regression model
factor_cov <- cov(X, use = "complete.obs")
cov_mat_multivar <- residual_vars + betas %*% factor_cov %*% t(betas)
print(dim(cov_mat_multivar))

# Check for positive definiteness
eigen_values <- eigen(cov_mat_multivar)$values
if (any(eigen_values <= 0)) {
  stop("Covariance matrix is not positive definite even after regularization!")
}

# Construct Minimum Variance Portfolio
D.mat <- 2 * cov_mat_multivar
d.vec <- rep(0, k)
A.mat <- cbind(rep(1, k), diag(k))
b.vec <- c(1, rep(0, k))

# Solve the Quadratic Programming Problem
qp.Result_multivar_minvar <- solve.QP(Dmat = D.mat, dvec = d.vec, Amat = A.mat, bvec = b.vec, meq = 1)

# Extract optimal weights
w_minvar_multivar <- round(qp.Result_multivar_minvar$solution, 5)

# Print weights
print(cbind((1:ncol(Y)), w_minvar_multivar))

# Calculate in-sample performance
PortReturn_minvar_multivar <- sum(m_vec_multivar * w_minvar_multivar)
PortRisk_minvar_multivar <- sqrt(t(w_minvar_multivar) %*% cov_mat_multivar %*% w_minvar_multivar)

print(round(cbind(PortReturn_minvar_multivar, PortRisk_minvar_multivar), 4))

#------------------------------------ OUT OF SAMPLE -----------------------------------------------

# Realized return of minimum-variance portfolio out-of-sample
RR_minvar_multivar_outofsample <- Y_outofsample %*% w_minvar_multivar
mean(RR_minvar_multivar_outofsample)
# Calculate portfolio risk (standard deviation) out-of-sample
cov_mat_multivar_outofsample <- residual_vars + betas %*% cov(X_outofsample) %*% t(betas)
PR_minvar_multivar_outofsample <- sqrt(t(w_minvar_multivar) %*% cov_mat_multivar_outofsample %*% w_minvar_multivar)

# Conditional Sharpe Ratio out-of-sample
CSR_minvar_multivar_outofsample <- mean(RR_minvar_multivar_outofsample) / PR_minvar_multivar_outofsample

# Calculate cumulative returns
Cum_Ret_multivar_minvar <- cumsum(RR_minvar_multivar_outofsample)

#==========================================================================
# PLOT CUMULATIVE RETURNS
#==========================================================================

# Cumulative Portfolio Returns
plot(dates, Cum_Ret_multivar_meanvar, type="l", col="blue", lwd=3, xlab="Time", ylab="Cumulative Return", 
     main="", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, ylim=c(-0.2, 0.6))
lines(dates, Cum_Ret_multivar_minvar, type="l", col="red", lwd=3)
legend("topleft", legend=c("Mean-Variance Portfolio", "Minimum Variance Portfolio"), 
       col=c("blue", "red"), lty=1, lwd=3, cex=1.5, inset=c(0.01, 0.01))

# Customize the x-axis to show the actual dates by year without duplication
axis.Date(1, at = seq(as.Date("2016-01-01"), as.Date("2019-07-01"), by = "years"), format = "%Y", cex.axis = 1.5)


#------------------------------------ d. ---------------------------------------------------------


#----------- CONSTANT CONDITIONAL CORRELATION (CCC) MODEL FOR THE VARIANVE COVARIANCE MATRIX -----------------

# in order to do this we need to modify the covariance matrix Σt to incorporate the structure
# Σt= Dt * R * Dt

#==============================================================================================
# CONSTRUCTION OF MEAN-VARIANCE PORTFOLIOS BASED ON CONSTANT CONDITIONAL CORRELATION (CCC) MODEL
# FOR THE VARIANVE COVARIANCE MATRIX
#=============================================================================================

# Construct the diagonal matrix Dt
# The diagonal matrix Dt is constructed using the square root of the diagonal elements of cov_mat_multivar
Dt <- diag(sqrt(diag(cov_mat_multivar)))
dim(Dt)

# Estimate the constant correlation matrix R
residual_matrix <- do.call(cbind, residuals_list)
R <- cor(residual_matrix, use = "complete.obs")
dim(R)

# Construct the new covariance matrix Sigma_t
cov_mat_CCC <- Dt %*% R %*% Dt
dim(cov_mat_CCC)

max(m_vec_multivar)
# Solve the optimization problem for the Mean-Variance Portfolio
targetreturn <- 0.01  # or your chosen target return
D.mat <- 2 * cov_mat_CCC
d.vec <- rep(0, k)
A.mat <- cbind(rep(1, k), m_vec_multivar, diag(k))
b.vec <- c(1, targetreturn, rep(0, k))

# Solve the Quadratic Programming Problem
qp.Result_meanvar <- solve.QP(Dmat = D.mat, dvec = d.vec, Amat = A.mat, bvec = b.vec, meq = 1)

# Extract optimal weights
w_meanvar <- round(qp.Result_meanvar$solution, 5)

# Calculate in-sample performance
PortReturn_ccc_meanvar <- sum(m_vec_multivar * w_meanvar)
PortRisk_ccc_meanvar <- sqrt(t(w_meanvar) %*% cov_mat_CCC %*% w_meanvar)

print(round(cbind(PortReturn_ccc_meanvar, PortRisk_ccc_meanvar), 4))

#==============================================================================================
# CONSTRUCTION OF MIN-VARIANCE PORTFOLIOS BASED ON CONSTANT CONDITIONAL CORRELATION (CCC) MODEL
# FOR THE VARIANVE COVARIANCE MATRIX
#=============================================================================================

# Construct the diagonal matrix Dt
#diagonal matrix Dt is constructed using the square root of the residual variances directly.

# Solve the optimization problem for the Minimum Variance Portfolio
D.mat <- 2 * cov_mat_CCC
d.vec <- rep(0, k)
A.mat <- cbind(rep(1, k), diag(k))
b.vec <- c(1, rep(0, k))

# Solve the Quadratic Programming Problem
qp.Result_minvar <- solve.QP(Dmat = D.mat, dvec = d.vec, Amat = A.mat, bvec = b.vec, meq = 1)
# Extract optimal weights
# the proportion of the portfolio invested in each fund
w_minvar <- round(qp.Result_minvar$solution, 5)

# Calculate in-sample performance
PortReturn_ccc_minvar <- sum(m_vec_multivar * w_minvar)
PortRisk_ccc_minvar <- sqrt(t(w_minvar) %*% cov_mat_CCC %*% w_minvar)

print(round(cbind(PortReturn_ccc_minvar, PortRisk_ccc_minvar), 4))

#==============================================================================================
# OUT-OF-SAMPLE PERFORMANCE EVALUATION
#==============================================================================================

# Calculate mean returns of factors out-of-sample
mean_factors_return_outofsample <- colMeans(X_outofsample, na.rm = TRUE)
mean_factors_return_outofsample <- matrix(mean_factors_return_outofsample, ncol = 1)

# Estimate mean returns of funds out-of-sample
m_vec_multivar_outofsample <- alphas + betas %*% mean_factors_return_outofsample

# Realized return of mean-variance portfolio out-of-sample
RR_ccc_meanvar_outofsample <- Y_outofsample %*% w_meanvar
mean(RR_ccc_meanvar_outofsample)
# Calculate portfolio risk (standard deviation) out-of-sample
cov_mat_CCC_outofsample <- Dt %*% R %*% Dt
PR_ccc_meanvar_outofsample <- sqrt(t(w_meanvar) %*% cov_mat_CCC_outofsample %*% w_meanvar)

# Conditional Sharpe Ratio out-of-sample
CSR_ccc_meanvar_outofsample <- mean(RR_ccc_meanvar_outofsample) / PR_ccc_meanvar_outofsample

# Realized return of minimum-variance portfolio out-of-sample
RR_ccc_minvar_outofsample <- Y_outofsample %*% w_minvar
mean(RR_ccc_minvar_outofsample)
# Calculate portfolio risk (standard deviation) out-of-sample
PR_ccc_minvar_outofsample <- sqrt(t(w_minvar) %*% cov_mat_CCC_outofsample %*% w_minvar)

# Conditional Sharpe Ratio out-of-sample
CSR_ccc_minvar_outofsample <- mean(RR_ccc_minvar_outofsample) / PR_ccc_minvar_outofsample

# Calculate cumulative returns
Cum_Ret_ccc_meanvar <- cumsum(RR_ccc_meanvar_outofsample)
Cum_Ret_ccc_minvar <- cumsum(RR_ccc_minvar_outofsample)

# Evaluate the portfolios
evaluations_CCC <- list(
  MeanVariance = list(
    PortReturn = round(PortReturn_meanvar, 4),
    PortRisk = round(PortRisk_meanvar, 4),
    RealizedReturn = round(mean(RR_ccc_meanvar_outofsample), 4),
    ConditionalSharpeRatio = round(CSR_ccc_meanvar_outofsample, 4),
    CumulativeReturn = round(Cum_Ret_ccc_meanvar[length(Cum_Ret_ccc_meanvar)], 4)
  ),
  MinVariance = list(
    PortReturn = round(PortReturn_minvar, 4),
    PortRisk = round(PortRisk_minvar, 4),
    RealizedReturn = round(mean(RR_ccc_minvar_outofsample), 4),
    ConditionalSharpeRatio = round(CSR_ccc_minvar_outofsample, 4),
    CumulativeReturn = round(Cum_Ret_ccc_minvar[length(Cum_Ret_ccc_minvar)], 4)
  )
)

print(evaluations_CCC)


# Cumulative Portfolio Returns
plot(dates, Cum_Ret_ccc_meanvar, type="l", col="blue", lwd=3, xlab="Time", ylab="Cumulative Return", 
     main="", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, ylim=c(-0.2, 0.6))
lines(dates, Cum_Ret_ccc_minvar, type="l", col="red", lwd=3)
legend("topleft", legend=c("Mean-Variance Portfolio", "Minimum Variance Portfolio"), 
       col=c("blue", "red"), lty=1, lwd=3, cex=1.5, inset=c(0.01, 0.01))

# Customize the x-axis to show the actual dates by year without duplication
axis.Date(1, at = seq(as.Date("2016-01-01"), as.Date("2019-07-01"), by = "years"), format = "%Y", cex.axis = 1.5)


#----------------------------------------- e.----------------------------------------------------

#------------ Fitting a Fama-French Three-Factor Model ------------------------------------------------------


#==============================================================================================
# CONSTRUCTION OF MEAN-VARIANCE PORTFOLIOS BASED ON FAMMA FRENCH 3 FACTOR MODEL
#=============================================================================================
# Ensure factors data contains columns: Mkt-RF, SMB, HML
#------------ Fitting a Fama-French Three-Factor Model ------------------------------------------------------


#==============================================================================================
# CONSTRUCTION OF MEAN-VARIANCE PORTFOLIOS BASED ON FAMA-FRENCH 3 FACTOR MODEL
#=============================================================================================
# Ensure factors data contains columns: Mkt-RF, SMB, HML
factors_3F <- factors[, c("Mkt-RF", "SMB", "HML")]

# Fit the Fama-French three-factor model for each fund
fit_ff_3F <- lapply(1:ncol(Ret), function(i) {
  lm(Ret[, i] ~ factors_3F[1:insample, "Mkt-RF"] + factors_3F[1:insample, "SMB"] + factors_3F[1:insample, "HML"])
})

# Extract the coefficients and residuals
alphas_3F <- sapply(fit_ff_3F, function(x) coef(x)[1])
betas_3F <- t(sapply(fit_ff_3F, function(x) coef(x)[-1]))
residuals_list_3F <- lapply(fit_ff_3F, residuals)
residual_vars_3F <- diag(sapply(residuals_list_3F, var))

# Calculate the mean returns of the factors
mean_factors_return_3F <- colMeans(factors_3F[1:insample, ])

# Ensure mean_factors_return_3F is a column vector
mean_factors_return_3F <- matrix(mean_factors_return_3F, ncol = 1)

# Estimate the mean returns of the funds
m_vec_ff_3F <- alphas_3F + betas_3F %*% mean_factors_return_3F

max(m_vec_ff_3F)

# Estimate the covariance matrix using the factor model
factor_cov_3F <- cov(factors_3F[1:insample, ])
cov_mat_ff_3F <- residual_vars_3F + betas_3F %*% factor_cov_3F %*% t(betas_3F)

# Solve the optimization problem for the Mean-Variance Portfolio
target_return_3F <- 0.007  # or your chosen target return
D.mat_3F <- 2 * cov_mat_ff_3F
d.vec_3F <- rep(0, ncol(Ret))
A.mat_3F <- cbind(rep(1, ncol(Ret)), m_vec_ff_3F, diag(ncol(Ret)))
b.vec_3F <- c(1, target_return_3F, rep(0, ncol(Ret)))

# Solve the Quadratic Programming Problem
qp.Result_meanvar_3F <- solve.QP(Dmat = D.mat_3F, dvec = d.vec_3F, Amat = A.mat_3F, bvec = b.vec_3F, meq = 1)

# Extract optimal weights
w_meanvar_3F <- round(qp.Result_meanvar_3F$solution, 5)

# Calculate in-sample performance
PortReturn_meanvar_3F <- sum(m_vec_ff_3F * w_meanvar_3F)
PortRisk_meanvar_3F <- sqrt(t(w_meanvar_3F) %*% cov_mat_ff_3F %*% w_meanvar_3F)

# Print portfolio return and risk
print(round(cbind(PortReturn_meanvar_3F, PortRisk_meanvar_3F), 4))

#------------------------------- OUT OF SAMPLE -----------------------------------------

# Assume factors_outofsample data contains columns: Mkt-RF, SMB, HML
factors_outofsample_3F <- factors_3F[(insample + 1):T, ]

# Calculate mean returns of factors out-of-sample
mean_factors_return_outofsample_3F <- colMeans(factors_outofsample_3F)

# Ensure mean_factors_return_outofsample_3F is a column vector
mean_factors_return_outofsample_3F <- matrix(mean_factors_return_outofsample_3F, ncol = 1)

# Estimate mean returns of funds out-of-sample
m_vec_ff_outofsample_3F <- alphas_3F + betas_3F %*% mean_factors_return_outofsample_3F

# Realized return of mean-variance portfolio out-of-sample
RR_ff_meanvar_outofsample_3F <- Ret_outofsample %*% w_meanvar_3F
mean(RR_ff_meanvar_outofsample_3F)
# Calculate portfolio risk (standard deviation) out-of-sample
cov_mat_ff_outofsample_3F <- residual_vars_3F + betas_3F %*% cov(factors_outofsample_3F) %*% t(betas_3F)
PR_ff_meanvar_outofsample_3F <- sqrt(t(w_meanvar_3F) %*% cov_mat_ff_outofsample_3F %*% w_meanvar_3F)

# Conditional Sharpe Ratio out-of-sample
CSR_ff_meanvar_outofsample_3F <- mean(RR_ff_meanvar_outofsample_3F) / PR_ff_meanvar_outofsample_3F

# Calculate cumulative returns
Cum_Ret_ff_meanvar_3F <- cumsum(RR_ff_meanvar_outofsample_3F)

#==============================================================================================
# CONSTRUCTION OF MIN-VARIANCE PORTFOLIOS BASED ON FAMA-FRENCH 3 FACTOR MODEL
#=============================================================================================
D.mat_minvar_3F <- 2 * cov_mat_ff_3F
d.vec_minvar_3F <- rep(0, ncol(Ret))
A.mat_minvar_3F <- cbind(rep(1, ncol(Ret)), diag(ncol(Ret)))
b.vec_minvar_3F <- c(1, rep(0, ncol(Ret)))

qp.Result_minvar_3F <- solve.QP(Dmat = D.mat_minvar_3F, dvec = d.vec_minvar_3F, Amat = A.mat_minvar_3F, bvec = b.vec_minvar_3F, meq = 1)

# Extract optimal weights 
w_minvar_3F <- round(qp.Result_minvar_3F$solution, 5)

# Calculate in-sample performance for Minimum Variance Portfolio
PortReturn_minvar_3F <- sum(m_vec_ff_3F * w_minvar_3F)
PortRisk_minvar_3F <- sqrt(t(w_minvar_3F) %*% cov_mat_ff_3F %*% w_minvar_3F)

# Print in-sample portfolio return and risk for Minimum Variance Portfolio
print(round(cbind(PortReturn_minvar_3F, PortRisk_minvar_3F), 4))

# ------------------------------ OUT OF SAMPLE -------------------------------------
# Factors out-of-sample data assumed to contain columns: Mkt-RF, SMB, HML
factors_outofsample_3F <- factors_3F[(insample + 1):T, ]

# Calculate mean returns of factors out-of-sample
mean_factors_return_outofsample_3F <- colMeans(factors_outofsample_3F)

# Ensure mean_factors_return_outofsample_3F is a column vector
mean_factors_return_outofsample_3F <- matrix(mean_factors_return_outofsample_3F, ncol = 1)

# Estimate mean returns of funds out-of-sample
m_vec_ff_outofsample_3F <- alphas_3F + betas_3F %*% mean_factors_return_outofsample_3F

# Realized return
RR_ff_minvar_outofsample_3F <- Ret_outofsample %*% w_minvar_3F
mean(RR_ff_minvar_outofsample_3F)
# Calculate portfolio risk (standard deviation) out-of-sample
cov_mat_ff_outofsample_3F <- residual_vars_3F + betas_3F %*% cov(factors_outofsample_3F) %*% t(betas_3F)
PR_ff_minvar_outofsample_3F <- sqrt(t(w_minvar_3F) %*% cov_mat_ff_outofsample_3F %*% w_minvar_3F)

# Conditional Sharpe Ratio out-of-sample
CSR_ff_minvar_outofsample_3F <- mean(RR_ff_minvar_outofsample_3F) / PR_ff_minvar_outofsample_3F

# Calculate cumulative returns for Minimum Variance Portfolio
Cum_Ret_ff_minvar_3F <- cumsum(RR_ff_minvar_outofsample_3F)


# Cumulative Portfolio Returns
plot(dates, Cum_Ret_ff_meanvar_3F, type="l", col="blue", lwd=3, xlab="Time", ylab="Cumulative Return", 
     main="", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, ylim=c(-0.2, 0.6))
lines(dates, Cum_Ret_ff_minvar_3F, type="l", col="red", lwd=3)
legend("topleft", legend=c("Mean-Variance Portfolio", "Minimum Variance Portfolio"), 
       col=c("blue", "red"), lty=1, lwd=3, cex=1.5, inset=c(0.01, 0.01))

# Customize the x-axis to show the actual dates by year without duplication
axis.Date(1, at = seq(as.Date("2016-01-01"), as.Date("2019-07-01"), by = "years"), format = "%Y", cex.axis = 1.5)


#==============================================================================================
# CONSTRUCTION OF MEAN-VARIANCE PORTFOLIOS BASED ON FAMA-FRENCH FOUR-FACTOR MODEL
#==============================================================================================

factors <- read_excel(file_path, sheet = "Factors")
factors <- as.data.frame(factors[,-1])
dim(factors)
head(factors)

# Ensure factors data contains columns: Mkt-RF, SMB, HML, MOM (Momentum)
factors_4F <- factors[, c("Mkt-RF", "SMB", "HML", "MOM")]

# Fit the four-factor model for each fund
fit_ff_4F <- lapply(1:ncol(Ret), function(i) {
  lm(Ret[, i] ~ factors_4F[1:insample, "Mkt-RF"] + factors_4F[1:insample, "SMB"] + factors_4F[1:insample, "HML"] + factors_4F[1:insample, "MOM"])
})

# Extract the coefficients and residuals
alphas_4F <- sapply(fit_ff_4F, function(x) coef(x)[1])
betas_4F <- t(sapply(fit_ff_4F, function(x) coef(x)[-1]))
residuals_list_4F <- lapply(fit_ff_4F, residuals)
residual_vars_4F <- diag(sapply(residuals_list_4F, var))

# Calculate the mean returns of the factors
mean_factors_return_4F <- colMeans(factors_4F[1:insample, ])

# Ensure mean_factors_return_4F is a column vector
mean_factors_return_4F <- matrix(mean_factors_return_4F, ncol = 1)

# Estimate the mean returns of the funds
m_vec_ff_4F <- alphas_4F + betas_4F %*% mean_factors_return_4F

max(m_vec_ff_4F)

# Estimate the covariance matrix using the factor model
factor_cov_4F <- cov(factors_4F[1:insample, ])
cov_mat_ff_4F <- residual_vars_4F + betas_4F %*% factor_cov_4F %*% t(betas_4F)

# Solve the optimization problem for the Mean-Variance Portfolio
target_return_4F <- 0.009  # or your chosen target return
D.mat_4F <- 2 * cov_mat_ff_4F
d.vec_4F <- rep(0, ncol(Ret))
A.mat_4F <- cbind(rep(1, ncol(Ret)), m_vec_ff_4F, diag(ncol(Ret)))
b.vec_4F <- c(1, target_return_4F, rep(0, ncol(Ret)))

# Solve the Quadratic Programming Problem
qp.Result_meanvar_4F <- solve.QP(Dmat = D.mat_4F, dvec = d.vec_4F, Amat = A.mat_4F, bvec = b.vec_4F, meq = 1)

# Extract optimal weights
w_meanvar_4F <- round(qp.Result_meanvar_4F$solution, 5)

# Calculate in-sample performance
PortReturn_meanvar_4F <- sum(m_vec_ff_4F * w_meanvar_4F)
PortRisk_meanvar_4F <- sqrt(t(w_meanvar_4F) %*% cov_mat_ff_4F %*% w_meanvar_4F)

# Print portfolio return and risk
print(round(cbind(PortReturn_meanvar_4F, PortRisk_meanvar_4F), 4))

#------------------------------- OUT OF SAMPLE -----------------------------------------

# Assume factors_outofsample data contains columns: Mkt-RF, SMB, HML, MOM
factors_outofsample_4F <- factors_4F[(insample + 1):T, ]

# Calculate mean returns of factors out-of-sample
mean_factors_return_outofsample_4F <- colMeans(factors_outofsample_4F)

# Ensure mean_factors_return_outofsample_4F is a column vector
mean_factors_return_outofsample_4F <- matrix(mean_factors_return_outofsample_4F, ncol = 1)

# Estimate mean returns of funds out-of-sample
m_vec_ff_outofsample_4F <- alphas_4F + betas_4F %*% mean_factors_return_outofsample_4F

# Realized return of mean-variance portfolio out-of-sample
RR_ff_meanvar_outofsample_4F <- Ret_outofsample %*% w_meanvar_4F
mean(RR_ff_meanvar_outofsample_4F)
# Calculate portfolio risk (standard deviation) out-of-sample
cov_mat_ff_outofsample_4F <- residual_vars_4F + betas_4F %*% cov(factors_outofsample_4F) %*% t(betas_4F)
PR_ff_meanvar_outofsample_4F <- sqrt(t(w_meanvar_4F) %*% cov_mat_ff_outofsample_4F %*% w_meanvar_4F)

# Conditional Sharpe Ratio out-of-sample
CSR_ff_meanvar_outofsample_4F <- mean(RR_ff_meanvar_outofsample_4F) / PR_ff_meanvar_outofsample_4F

# Calculate cumulative returns
Cum_Ret_ff_meanvar_4F <- cumsum(RR_ff_meanvar_outofsample_4F)

#==============================================================================================
# CONSTRUCTION OF MIN-VARIANCE PORTFOLIOS BASED ON FAMA-FRENCH FOUR-FACTOR MODEL
#==============================================================================================
D.mat_minvar_4F <- 2 * cov_mat_ff_4F
d.vec_minvar_4F <- rep(0, ncol(Ret))
A.mat_minvar_4F <- cbind(rep(1, ncol(Ret)), diag(ncol(Ret)))
b.vec_minvar_4F <- c(1, rep(0, ncol(Ret)))

qp.Result_minvar_4F <- solve.QP(Dmat = D.mat_minvar_4F, dvec = d.vec_minvar_4F, Amat = A.mat_minvar_4F, bvec = b.vec_minvar_4F, meq = 1)

# Extract optimal weights 
w_minvar_4F <- round(qp.Result_minvar_4F$solution, 5)

# Calculate in-sample performance for Minimum Variance Portfolio
PortReturn_minvar_4F <- sum(m_vec_ff_4F * w_minvar_4F)
PortRisk_minvar_4F <- sqrt(t(w_minvar_4F) %*% cov_mat_ff_4F %*% w_minvar_4F)

# Print in-sample portfolio return and risk for Minimum Variance Portfolio
print(round(cbind(PortReturn_minvar_4F, PortRisk_minvar_4F), 4))

#------------------------------- OUT OF SAMPLE -------------------------------------
# Factors out-of-sample data assumed to contain columns: Mkt-RF, SMB, HML, MOM
factors_outofsample_4F <- factors_4F[(insample + 1):T, ]

# Calculate mean returns of factors out-of-sample
mean_factors_return_outofsample_4F <- colMeans(factors_outofsample_4F)

# Ensure mean_factors_return_outofsample_4F is a column vector
mean_factors_return_outofsample_4F <- matrix(mean_factors_return_outofsample_4F, ncol = 1)

# Estimate mean returns of funds out-of-sample
m_vec_ff_outofsample_4F <- alphas_4F + betas_4F %*% mean_factors_return_outofsample_4F

# Realized return of minimum-variance portfolio out-of-sample
RR_ff_minvar_outofsample_4F <- Ret_outofsample %*% w_minvar_4F
mean(RR_ff_minvar_outofsample_4F)
# Calculate portfolio risk (standard deviation) out-of-sample
cov_mat_ff_outofsample_4F <- residual_vars_4F + betas_4F %*% cov(factors_outofsample_4F) %*% t(betas_4F)
PR_ff_minvar_outofsample_4F <- sqrt(t(w_minvar_4F) %*% cov_mat_ff_outofsample_4F %*% w_minvar_4F)

# Conditional Sharpe Ratio out-of-sample
CSR_ff_minvar_outofsample_4F <- mean(RR_ff_minvar_outofsample_4F) / PR_ff_minvar_outofsample_4F

# Calculate cumulative returns for Minimum Variance Portfolio
Cum_Ret_ff_minvar_4F <- cumsum(RR_ff_minvar_outofsample_4F)



# Cumulative Portfolio Returns
plot(dates, Cum_Ret_ff_meanvar_4F, type="l", col="blue", lwd=3, xlab="Time", ylab="Cumulative Return", 
     main="", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, ylim=c(-0.2, 0.6))
lines(dates, Cum_Ret_ff_minvar_4F, type="l", col="red", lwd=3)
legend("topleft", legend=c("Mean-Variance Portfolio", "Minimum Variance Portfolio"), 
       col=c("blue", "red"), lty=1, lwd=3, cex=1.5, inset=c(0.01, 0.01))

# Customize the x-axis to show the actual dates by year without duplication
axis.Date(1, at = seq(as.Date("2016-01-01"), as.Date("2019-07-01"), by = "years"), format = "%Y", cex.axis = 1.5)






