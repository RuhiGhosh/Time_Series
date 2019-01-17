

#================================================================================
#              Time Series Analysis for Traffic Data             
#==================================================================================

library(forecast)
#loading data
df=read.csv('C:/Users/91944/Documents/R/praxis/Time series/Traffic.csv',stringsAsFactors = FALSE)  #In data frame string treated as factor is set to false
View(df)    #View date
dim(df)     #Dimensions= 747 * 2
ndf=df$X..Vehicles.in.tunnel   #Storing in new dataframe
df$Day=as.Date(df[,1],format='%d-%b-%y')  #information about date convertion format ->  https://www.stat.berkeley.edu/~s133/dates.html
class(df)  #Checking type
df[1,1] #check starting date
k=ts(ndf,start = c(2003,11,01),freq=365) #convert to ts and store
class(k)   #check class   
View(k)    #view data
start(k); end(k); frequency(k)    #Check start,end dates and frequency
plot(k, ylab = "Vehicles in tunnel")   #Plotting data
#Clear view of trend,seasonal effect removed - aggregate
plot(aggregate(k)) #Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.

par(new=TRUE)   #Combining both plots

#summary of the values for each season can be viewed
#using a boxplot, with the cycle function being used to extract the seasons
#for each item of data. 
boxplot(k ~ cycle(k),axes=FALSE,ylab='')

plot(decompose(k))    #see trend and seasonality
g=diff(k)             #First order diff  for making trend stationary for ARIMA
plot(g)                #plot first order dif
plot(decompose(g))     #Decompose after differencing


#==========================================================================
#                               SMA (3,6,12 days)
#===========================================================================


library(TTR)       

tra_sma3 <-SMA(g,3)       #3 days SMA
tra_sma3
plot.ts(tra_sma3)         #Plot 6 days SMA   
head(tra_sma3)
plot(forecast(tra_sma3))  #Plot of forecasted SMA 


tra_sma6 <-SMA(g,6)       #6 days SMA
plot.ts(tra_sma6)         #Plot of 6 days SMA
head(tra_sma6)          


tra_sma6 <-SMA(g,12)      #12 days SMA
plot.ts(tra_sma6)         #Plot
head(tra_sma6)

#==========================================================================
#                               EMA (3,6,12 days)
#===========================================================================

tra_ema3 <-EMA(g,3)       #3 days EMA
plot.ts(tra_ema3)         #Plot
head(tra_ema3)

tra_ema3 <-EMA(g,6)      #6 days EMA
plot.ts(tra_ema3)        #Plot 
head(tra_ema3)
plot(forecast(tra_ema3))  #Forecasted plot for EMA


#Seasonal Plot
seasonplot(k,
           col = c("red","blue","green"),
           bty ="l",
           type = "l",
           year.labels = T,main = 'Seasonal plot')
legend("topleft", legend=c("2003", "2004","2005"),
       col=c("red", "blue","green"), lty=1:2, cex=0.4,box.lty=2)



#par(mfrow=c(1,1))
#plot(k)
#tsdisplay(k)        
library(tseries)

adf.test(k) # Stationarity   #since our p-value is 0.01 we will reject the null hypo of non stationary...our data is stationary

tsdisplay(g) # Autocorrelation
## pacf :  Degree of association between two variables while adjusting for effect 
## of one or more additional variable
## acf  :  Similarity between values of the same variable across observations 
## pacf is used for p (auto regressive component which is regression of value with lag time) and 
## acf is used for ## q (moving average component which is regression of error with its lag)


## If acf has less bars outside the boundary compared to pacf, then, 
## we will use acf which is q and p =0 

## If pacf has less bars outside the boundary compared to acf, then, 
## we will use pacf which is p and q =0

#====================================================================
#             ADF Test for stationarity check
#====================================================================
adf.test(k)


#====================================================================
#     Auto ARIMA MODEL
#=====================================================================

mode<-auto.arima(g)
refit<-Arima(g,model=mode)
accuracy(refit)               #to check the accuracy


# Upper case P,D,Q is for seasonal component of ARIMA model
# Parameters of ARIMA model

auto.arima(g, trace = T)   #(4,0,3)  is the best values for arima   (ar is pacf,,,,ma is acf)

#=====================================================================
#                        Holts Winter
#====================================================================


# Phi auto generated
plot(holt(k, h = 90, damped = T))     #holts damped
# To see the generated value for phi
summary(holt(k, h = 90, damped = T))

# Overview plot - models
holttrend = holt(g, h = 90)           #holts trend
holtdamped = holt(g, h = 90, damped = T)        #holts damped 
arimafore = forecast(auto.arima(g), h = 90)     # forecast of ARIMA for next 90 days
accuracy(holttrend)                             #Accuracy check for holttrend
accuracy(holtdamped)                            #Accuracy check for holtdamped

library(ggplot2)
# 3 Forecast Lines as Comparison
autoplot(g) +
  forecast::autolayer(holttrend$mean, series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$mean, series = "Holt Damped Trend") +
  forecast::autolayer(arimafore$mean, series = "ARIMA") +
  xlab("year") + ylab("Vechicles") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Traffic") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "blue",
                                                      face="bold", size=15))

#================================================================================
#                                 Naive
#===================================================================================

n=forecast(summary(naive(k,h=80)))
autoplot(n ,ylab='# Vehicles')

#==============================================================================
#                                     END
#===============================================================================
