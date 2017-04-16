library(forecast)
library(vars)
library(hydroGOF)
library("tseries")
library(car)


#### DEFINE A PLOT FUNCTION THAT TAKES THE MODEL with br_train #######
make_plot<-function(model,DF=NULL, main_title="Canadian Monthly Bankruptcy rate"){
  # Used in ARIMA, ARIMAX, HoltWinter (use forecast)
  # Given a model based on the entire dataset, output the forecast plot

  if (is.null(DF)==FALSE){
    print("adding xreg")
    f<-forecast(model,h=24,level=0.95,xreg=DF)
  }
  else{
    print(model)
  f<-forecast(model,h=24,level=0.95)
  }
  l<-ts(f$lower,start=c(2011,1),frequency=12)
  h<-ts(f$upper,start=c(2011,1),frequency=12)
  pred_ARIMA<-exp(f$mean)
  print(pred_ARIMA)
#  par(mfrow=c(1,1))
  plot(br_train, xlim=c(1987,2012), ylim=c(0,0.06), 
       main = main_title, 
       ylab = "Monthly bankruptcy rate", xlab = "Month")
  abline(v = 2010, lwd = 1, col = "black")  
  points(pred_ARIMA, type = "l", col = "blue")
  points(exp(l), type = "l", col = "grey")
  points(exp(h), type = "l", col = "grey")
  points(exp(f$fitted),type="l", col = "green")
  
  legend(1988,0.05,c('forecast','model fit','95%CI'),
         lwd=c(2.5,2.5),col=c('blue','green','grey'))
}

###### DEFINE A FUNCTION TO CALCULATE RMSE for model_80 #####
get_RMSE<-function(model_80,DF=NULL){
  # Used in ARIMA, ARIMAX, HoltWinter
  # Given the model based on the 80% of the training set, output a RMSE score
  
  if (is.null(DF)==FALSE){
    print("ARIMAX-adding xreg")
    f<-forecast(model_80,h=48,level=0.95,xreg=DF)
  }
  else{
    f<-forecast(model_80,h=48,level=0.95)
  }
  pred_ARIMA<-exp(f$mean)
  RMSE<-rmse(pred_ARIMA,br_train20)
  print(paste("RMSE",RMSE,sep=':'))
}

# Accurately forecasting national bankruptcy rates is of interest to national banks,
# insurance companies, credit-lenders, politicians etc. The goal of this project will be to
# precisely and accurately forecast monthly bankruptcy rates for Canada. 

#In the file “train.csv” you will find monthly data from January 1987 to December 2010 on the
# following variables:
# Unemployment Rate
# Population
# Bankruptcy Rate
# Housing Price Index

####### load test data: to forecast the January 2011 – December 2012 bankruptcy rates. #######
df_test<-read.csv('test.csv')
#Note, test set does not provide bankruptcy rate
#df_test
ur_test<-df_test$Unemployment_Rate
po_test<-df_test$Population
hp_test<-df_test$House_Price_Index

ur_test<-ts(ur_test,start=c(2011,1),end=c(2012,12),frequency = 12)
po_test<-ts(po_test,start=c(2011,1),end=c(2012,12),frequency = 12)
hp_test<-ts(hp_test,start=c(2011,1),end=c(2012,12),frequency = 12)

####### load train data:monthly data from January 1987 to December 2010 ########
df_train<-read.csv('train.csv')
#str(df_train)

ur_train<-df_train$Unemployment_Rate
po_train<-df_train$Population
br_train<-df_train$Bankruptcy_Rate
hp_train<-df_train$House_Price_Index

ur_train<-ts(ur_train,start=c(1987,1),end=c(2010,12),frequency = 12)
po_train<-ts(po_train,start=c(1987,1),end=c(2010,12),frequency = 12)
br_train<-ts(br_train,start=c(1987,1),end=c(2010,12),frequency = 12)
hp_train<-ts(hp_train,start=c(1987,1),end=c(2010,12),frequency = 12)

par(mfrow=c(3,1))
plot(br_train,main="bankruptcy rate")
plot(ur_train,main='unemployment rate')
#plot(po_train, main='population rate')
plot(hp_train, main='housing price index') 
#from these four plots, housing price index looks the most promising


#Split training set into first 20 years and last 4 years 
#in order to apply RMSE method for model evaluation and selection

######Note, use br_train80 for model prediction and br_train20 for RMSE calculation ####
n<-length(br_train)
br_train80<-ts(br_train[1:240], start=c(1987,1),end=c(2006,12),frequency = 12)  #I am using the first 20 years as training set (1987.1-2006.12)
#length(ur_train80) #240- 20 years
br_train20<-ts(br_train[241:n],start=c(2007,1),end=c(2010,12),frequency = 12) # I am using the last 4 years as test set (2007.1-2010.12)
#length(ur_train20) #48 - 4 years

#split the same way for the other 3 variables (for later use of SERIMAX & VAR modeling)
ur_train80<-ts(ur_train[1:240], start=c(1987,1),end=c(2006,12),frequency = 12)  
ur_train20<-ts(ur_train[241:n],start=c(2007,1),end=c(2010,12),frequency = 12) 

po_train80<-ts(po_train[1:240], start=c(1987,1),end=c(2006,12),frequency = 12)  
po_train20<-ts(po_train[241:n],start=c(2007,1),end=c(2010,12),frequency = 12)

hp_train80<-ts(hp_train[1:240], start=c(1987,1),end=c(2006,12),frequency = 12) 
hp_train20<-ts(hp_train[241:n],start=c(2007,1),end=c(2010,12),frequency = 12)


###### MODEL- PREPROCESSING- EXPLORATING THE DATA ########
par(mfrow=c(2,1))
plot(br_train, main='Canadian bankruptcy rate') #perform log transform to y
plot(diff(br_train))  #clearly heteroscadacity, therefore, log transformation is necessary

#log transform & remove trend difference
plot(log(br_train), main='Canadian bankruptcy rate - after log transformation') #after log transformation, variance looks a lot more reasonable

plot(diff(log(br_train)), main='Residuals after log transformation and removing trend and seasonality') #looks good
abline(h=0,col='red')



ndiffs(log(br_train)) #1 d=1
nsdiffs(diff(log(br_train))) #no clear seasonal differencing needed 
adf.test(diff(log(br_train))) 
# Dickey-Fuller = -7.3221, Lag order = 6, p-value = 0.01
# alternative hypothesis: stationary

# from both adf test and nsdiff, it looks like, 
#1. after ordinary differencing, data is already stationary
#2. we no longer need to do seasonal differencing, nsdiff=0
# d=1, D=0

br<-diff(log(br_train))  #ok
plot(br, main='Canadian Bankruptcy training data after differencing')
abline(h=0,col='red')

##### NEXT QUESTION, NOW br is STATIONARY, HOW TO MODEL IT? #######
par(mfrow=c(2,1))
acf(br,main='acf plot',lag.max = 276)  #br has already been differenced (q<=6)
pacf(br,main='pacf plot',lag.max = 276) 

acf(br,main='acf plot',lag.max = 48)  #br has already been differenced (q<=3, Q<=2)
pacf(br,main='pacf plot',lag.max = 48) #p<6, P<=1

######### ARIMA ##########

# m_s<-arima(log(br_train),order=c(3,1,1), seasonal=list(order=c(1,1,0),period=12), method='CSS')
# m_s
# tsdiag(m_s)
# 
# m_s80<-arima(log(br_train80),order=c(8,1,0), seasonal=list(order=c(6,1,0),period=12), method='CSS')
# m_s80
# tsdiag(m_s)
# get_RMSE(m_s80) #[1] "RMSE:0.0106937967236325"

m6_80<-arima(log(br_train80), order=c(3,1,1),seasonal =list(order=c(6,0,0),period=12),method='CSS') 
summary(m6_80)
m6_80
get_RMSE(m6_80)  #0.007267
tsdiag(m6_80)

# Make the prediction plot
m6_total<-arima(log(br_train), order=c(2,1,0),seasonal = list(order=c(4,1,0),period=12),method='CSS')
m6_total #sigma^2 estimated as 0.003541:  part log likelihood = 385.75
tsdiag(m6_total) #good
par(mfrow=c(1,1))
qqPlot(m6_total$residuals) # normality assumption is not met

par(mfrow=c(3,1))
make_plot(m6_total,main_title = "Canadian Monthly Bankruptcy rate- ARIMA")
#print out tubular result as well as forcast plot
get_RMSE(m6_80) #[1] "RMSE:0.00745430060005839"


######### ARIMAX ##########

# calculate RMSE
m6_80X_hp_po_ur2<-arima(log(br_train80), order=c(2,1,0),seasonal = list(order=c(5,1,0),period=12),xreg=data.frame(log(hp_train80),log(po_train80),log(ur_train80)),method='CSS')
summary(m6_80X_hp_po_ur2)
#sigma^2 estimated as 0.00284:  part log likelihood = 343.45
print ("using population rate & housing index rate & unemployment rate")
get_RMSE(m6_80X_hp_po_ur2,DF=data.frame(log(hp_train20),log(po_train20),log(ur_train20))) 
#"RMSE:0.00361084660638334"

# make adjustment to fix the tailing tail
m6_total_hp_po_ur2<-arima(log(br_train), order=c(2,1,0),seasonal = list(order=c(5,1,0),period=12),xreg=data.frame(log(hp_train),log(po_train),log(ur_train)),method='CSS')
m6_total_hp_po_ur2 #sigma^2 estimated as 0.00293:  part log likelihood = 411.8
tsdiag(m6_total_hp_po_ur2) 
par(mfrow=c(4,1))
make_plot(m6_total_hp_po_ur2,DF=data.frame(log(hp_test),log(po_test),log(ur_test)),main_title = "Canadian Monthly Bankruptcy rate- ARIMAX") 

########### Holt Winters #############
hw.AP_auto<-HoltWinters(log(br_train), seasonal = 'multi')
hw.AP_auto
# Smoothing parameters:
# alpha: 0.3620945
# beta : 0.1616889
# gamma: 0.1870295

#get RMSE
# hw.AP_80_auto<-HoltWinters(log(br_train80), seasonal = 'multi')
# get_RMSE(hw.AP_80_auto) #[1] "RMSE:0.00887543465999917"
# #plot the forecast
# make_plot(hw.AP_auto, main='main_title = "Canadian Monthly Bankruptcy rate- ARIMA- HoltWinters')
# # Smoothing parameters:
# # alpha: 0.3620945
# # beta : 0.1616889
# # gamma: 0.1870295

#use forcast 
hw.AP<-HoltWinters(log(br_train),alpha=0.2, beta=0.6, gamma=0.1, seasonal = 'multi')
hw.AP

#get RMSE
hw.AP_80<-HoltWinters(log(br_train80),alpha=0.2, beta=0.6, gamma=0.1, seasonal = 'multi')
get_RMSE(hw.AP_80) #[1] "RMSE:0.00374121123743764"
#plot the forecast
make_plot(hw.AP, main='Canadian Monthly Bankruptcy rate- HoltWinters')

######### VAR ########### 
# not using forecast and therefore, plot and rmse function is not used
# VARselect(y = data.frame(log(br_train), log(hp_train),log(po_train),log(ur_train)))
# # $selection
# # AIC(n)  HQ(n)  SC(n) FPE(n) 
# # 10     10      6     10 
# 
# m_var10<-VAR(y = data.frame(log(br_train), log(hp_train),log(po_train),log(ur_train)), p = 10)
# summary(m_var10)
# class(m_var10$y)
# plot(m_var10)
# VAR_fitted<-ts(m_var10$y[,1],start=c(1987,1),end=c(2010,12),frequency = 12)
# #Log Likelihood: 4805.674 
# # Residual standard error: 0.06627 on 237 degrees of freedom
# # Multiple R-Squared: 0.9747,	Adjusted R-squared: 0.9704 
# # F-statistic:   228 on 40 and 237 DF,  p-value: < 2.2e-16 
# 
# #str(predict(m_var10,n.ahead = 24,ci=0.95))
# 
# ###### CALCULATE RMSE ##########
# m_var10_rmse<-VAR(y = data.frame(log(br_train80), log(hp_train80),log(po_train80),log(ur_train80)), p = 10)
# #str(predict(m_var10,n.ahead = 24,ci=0.95))
# 
# pred_VAR_rmse<-predict(m_var10_rmse,n.ahead = 48,ci=0.95)$fcst$log.br_train
# pred_VAR_mean_rmse<-exp(pred_VAR_rmse[,1]) #return me a matrix
# RMSE_VAR_rmse<-rmse(pred_VAR_mean_rmse, br_train20)
# print (paste("predicted RMSE", RMSE_VAR_rmse,sep=': ')) 
# #[1] "predicted RMSE: 0.00410080572546996"  #lowest RMSE so far
# 
# 

## TEST ALTERNATIVE MODEL 
VARselect(y = data.frame(log(br_train), log(hp_train),log(ur_train)))
# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 6      5      3      6 
m_var<-VAR(y = data.frame(log(br_train), log(hp_train),log(ur_train)), p = 3)
summary(m_var)
plot(m_var)



VAR_fitted<-ts(m_var$y[,1],start=c(1987,1),end=c(2010,12),frequency = 12)

#Log Likelihood: 2160.571
# Multiple R-Squared: 0.9395,	Adjusted R-squared: 0.9375 
# F-statistic: 474.7 on 9 and 275 DF,  p-value: < 2.2e-16 

###### CALCULATE RMSE ##########
m_var_rmse<-VAR(y = data.frame(log(br_train80), log(hp_train80),log(ur_train80)), p = 3)
#str(predict(m_var10,n.ahead = 24,ci=0.95))

pred_VAR_rmse<-predict(m_var_rmse,n.ahead = 48,ci=0.95)$fcst$log.br_train
pred_VAR_mean_rmse<-exp(pred_VAR_rmse[,1]) #return me a matrix
RMSE_VAR_rmse<-rmse(pred_VAR_mean_rmse, br_train20)
print (paste("predicted RMSE", RMSE_VAR_rmse,sep=': ')) 
#[1] "predicted RMSE: 0.00456726886317469"


#### MAKE FORECAST PLOT #####
par(mfrow=c(1,1))
pred_VAR<-predict(m_var,n.ahead = 24,ci=0.95)$fcst$log.br_train  #return me a matrix
pred_mean<-pred_VAR[,1]
pred_mean<-ts(pred_mean,start=c(2011,1),frequency=12)
l<-ts(pred_VAR[,2],start=c(2011,1),frequency=12)
h<-ts(pred_VAR[,3],start=c(2011,1),frequency=12)
plot(br_train, xlim=c(1987,2012), ylim=c(0,0.06),
     main = "Canadian Monthly Bankruptcy rate- VAR",
     ylab = "Monthly bankruptcy rate", xlab = "Month",lwd=2)
#VAR_fitted
points(exp(VAR_fitted),col='green',type='l', lwd=1)
abline(v = 2011, lwd = 1, col = "black")
points(exp(pred_mean), type = "l", col = "blue")
points(exp(l), type = "l", col = "grey")
points(exp(h), type = "l", col = "grey")

legend(1988,0.05,c('forecast','95%CI','model fit'),
       lwd=c(2.5,2.5),col=c('blue','grey','green'))

# write this forecast to a csv
pred_VAR_ts<-ts(exp(pred_VAR),start=c(2011,1),frequency=12)
write.csv(pred_VAR_ts, file="forecast.csv")
