

# Q1 ----------------------------------------------------------------------

#Set the seed (1 can be replaced with a number of your choice)
set.seed(1)

#Simulate 1000 observations from the ARMA(2,1) process with
#phi1=0.7, phi2=0.2, theta1=0.6 and sigma_z=1
x<-arima.sim(n=1000,model=list(ar=c(0.7,0.2),ma=c(0.6)),sd=1)

#(b)
#Produce a time plot for the simulated data 'x'
ts.plot(x)

#(c)
#Set the R graphics device to contain two plots (1 row, 2 columns)
#x11()
par(mfrow=c(1,2))
#Plot the sample ACF
acf(x)

#Plot the sample PACF
#ylim=c(-1,1) ensures that the vertical axis runs from -1 to 1.
pacf(x,ylim=c(-1,1))

#(d) Theoretical ACF and PACF
#Calculate the theoretical ACF for the ARMA(2,1) process from (a)
#call this t_acf
t_acf<-ARMAacf(ar=c(0.7,0.2),ma=c(0.6),lag.max=30)
#Calculate the theoretical PACF for the ARMA(2,1) process from (a)
#call this t_pacf
t_pacf<-ARMAacf(ar=c(0.7,0.2),ma=c(0.6),lag.max=30,pacf=TRUE)

#(e) Compare theoretical ACF,PACF with sample ACF and PACF

#Set the R graphics device to contain two plots (1 row, 2 columns)
#x11()
par(mfrow=c(1,2))
#Plot the sample ACF
acf(x)
#Add the theoretical ACF to this plot as a red line
lines(c(0:30),t_acf,col="red")
#Plot the sample PACF
pacf(x,ylim=c(-1,1))
#Add the theoretical PACF to this plot as a red line
lines(c(1:30),t_pacf,col="red")


# Q2 ----------------------------------------------------------------------

#Simulate 1000 observations from the ARMA(2,1) process with
#phi1=0.7, phi2=0.2, theta1=0.6 and sigma_z=1
x2<-arima.sim(n=10000,model=list(ar=c(-0.8),ma=c(0.4)),sd=0.8)

par(mfrow=c(1,1))

ts.plot(x2)

#Calculate the theoretical ACF
t_acf_x2<-ARMAacf(ar=c(-0.8),ma=c(0.4),lag.max=30)

#Calculate the theoretical PACF
t_pacf_x2<-ARMAacf(ar=c(-0.8),ma=c(0.4),lag.max=30,pacf=TRUE)

par(mfrow=c(1,2))
#Plot the sample ACF
acf(x2)
#Add the theoretical ACF to this plot as a red line
lines(c(0:30),t_acf_x2,col="red")
#Plot the sample PACF
pacf(x2,ylim=c(-1,1))
#Add the theoretical PACF to this plot as a red line
lines(c(1:30),t_pacf_x2,col="red")


# Q3 ----------------------------------------------------------------------

practical2 <- read.csv("~/Documents/GitHub/Statistics/TimeSeries/Computer2/practical2.csv", sep="",header = TRUE)
#Declare the dataset to be time series data
practical2<-ts(practical2$y,start=1,end=2000)

#Produce a time plot for the simulated data 'x'
ts.plot(practical2)

#Take the first difference of the dataset "practical2"
y2<-diff(practical2)

ts.plot(y2)

#Plot the sample ACF,PACF
par(mfrow=c(1,2))
acf(y2)
pacf(y2)

#Calculate the theoretical ACF
t_acf_y2<-ARMAacf(ar=c(0.5),lag.max=30)

#Calculate the theoretical PACF
t_pacf_y2<-ARMAacf(ar=c(0.5),lag.max=30,pacf=TRUE)

par(mfrow=c(1,2))
#Plot the sample ACF
acf(y2)
#Add the theoretical ACF to this plot as a red line
lines(c(0:30),t_acf_y2,col="red")
#Plot the sample PACF
pacf(y2,ylim=c(-1,1))
#Add the theoretical PACF to this plot as a red line
lines(c(1:30),t_pacf_y2,col="red")
