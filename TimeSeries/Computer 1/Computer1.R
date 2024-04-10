
# Q1(a) -------------------------------------------------------------------


#Set the seed (7436 can be replaced with a number of your choice)
set.seed(7436)
#Assign a value for the parameter phi1
phi1<-0.9
#Assign a value for the parameter sigma_z
sigma_z<-0.5
#Simulate 1000 observations from the AR(1) model with
#phi1=0.9 and sigma_z=0.5
y<-arima.sim(n=1000,model=list(ar=c(phi1)),sd=sigma_z)


# Q1(b) -------------------------------------------------------------------

#Produce a time plot of the simulated AR(1) data
ts.plot(y)


# Q1(c) -------------------------------------------------------------------

#Produce a plot of the sample ACF for the simulated AR(1) data
acf(y)

# Q1(d) -------------------------------------------------------------------

#Function to calculate the theoretical ACF for the AR(1) process
Q1.AR1.acf<-ARMAacf(ar=c(phi1),ma=c(0),lag.max=100)


# Q1(e) -------------------------------------------------------------------

#Produce a plot of the sample ACF for the simulated AR(1) data
acf(y)
#Add a line (in red) that shows the theoretical ACF against lags 0 to 100
y_lag<-c(0:100)
lines(y_lag,Q1.AR1.acf,col="red")

y<-arima.sim(n=10000,model=list(ar=c(phi1)),sd=sigma_z)

#Produce a plot of the sample ACF for the simulated AR(1) data
acf(y)
#Add a line (in red) that shows the theoretical ACF against lags 0 to 100
y_lag<-c(0:100)
lines(y_lag,Q1.AR1.acf,col="red")


y<-arima.sim(n=100000,model=list(ar=c(phi1)),sd=sigma_z)

#Produce a plot of the sample ACF for the simulated AR(1) data
acf(y)
#Add a line (in red) that shows the theoretical ACF against lags 0 to 100
y_lag<-c(0:100)
lines(y_lag,Q1.AR1.acf,col="red")



# Q2(a) --------------#Set the seed of your choice
set.seed(14102)
#Simulate 1500 observations from the MA(1) model with
#theta1=0.6 and sigma_z=0.7
x<-arima.sim(n=1500,model=list(ma=c(0.6)),sd=0.7)


# Q2(b) -------------------------------------------------------------------

#Code to produce the time plot
ts.plot(x)

#Code to calculate the theoretical ACF for the MA(1) process
Q2.MA1.acf<-ARMAacf(ar=c(0),ma=c(0.6),lag.max=100)
#Produce a plot of the sample ACF for the simulated AR(1) data
acf(x,main="Sample ACF (MA(1) process, theta1 = 0.6)")
#Add a line (in red) that shows the theoretical ACF against lags
#0 to 100.
x_lag<-c(0:100)
lines(x_lag,Q2.MA1.acf,col="red")


# Q3(a) -------------------------------------------------------------------

#Simulate time series data manually

#Set the seed (82301 may be replaced by a number of your choice).
set.seed(82301)
#State the required number of of simulated values (n=1000).
n<-1000
#Define an empty vector of length n which will eventually contain
#the simulated time series values.
x<-numeric(n)
#Define the values of phi1 and sigma_z
phi1<--0.6
sigma_z<-0.5
#Define an nx1 vector of independent and identically distributed white
#noise terms: z_1,...,z_n. Here, we assume that the white noise terms
#are normally distributed.
z<-rnorm(n,mean=0,sd=sigma_z)

#Set the first value of the series (first element of x) to be random
#white noise (z_1).
x[1]<-z[1]
#Values 2:n of the vector x will depend on previous values of the
#series, according to the AR(1) model. We use the 'for loop' below to
#populate values of x.
for(t in 2:n){
  x[t]<-phi1*x[t-1]+z[t]
}
#At this point, we should tell R that the data we've simulated are time
#series data. We do this by running the command 'ts' below. This is known
#as creating a 'time series object'. 'start' defines the time point at
#which the series starts and 'end' that where the series ends.
x<-ts(x,start=1,end=n)


# Q3(b) -------------------------------------------------------------------

#Code to produce the time plot
ts.plot(x)


# Q3(c) -------------------------------------------------------------------

#Produce a plot of the sample ACF for the simulated AR(1) data
acf(x,main="Sample ACF (AR(1) process, phi1 = -0.6)")
#Code to calculate the theoretical ACF for the A1(1) process
Q3.AR1.acf<-ARMAacf(ar=c(-0.6),ma=c(0),lag.max=100)
#Add a line (in red) that shows the theoretical ACF against lags
#0 to 100
x_lag<-c(0:100)
lines(x_lag,Q3.AR1.acf,col="red")



# Q4(a) -------------------------------------------------------------------

#Set the seed
set.seed(9345)
#State the required number of of simulated values (n=2000).
n<-2000
#Define an empty vector of length n which will eventually contain
#the simulated time series values.
x<-numeric(n)
#Define the values of phi1 and sigma_z
theta1<-0.8
sigma_z<-sqrt(0.64)
#Define an nx1 vector of independent and identically distributed white
#noise terms: z_1,...,z_n. Here, we assume that the white noise terms
#are normally distributed.
z<-rnorm(n,mean=0,sd=sigma_z)
#Set the first value of the series (first element of z) to be the random
#white noise term (z_1).
x[1]<-z[1]
#Values 2:n of the vector x will depend on previous values of the
#series, according to the MA(1) model. We use the `for loop' below to
#populate values of x.
for(t in 2:n){
  x[t]<-theta1*z[t-1]+z[t]
}
#We define the simulate data to be a time series object.
x<-ts(x,start=1,end=n)


# Q4(b) -------------------------------------------------------------------

#Code to produce the time plot
ts.plot(x)


# Q4(c) -------------------------------------------------------------------

#Produce a plot of the sample ACF for the simulated MA(1) data
acf(x,main="Sample ACF (MA(1) process, theta1 = 0.8)")
#Code to calculate the theoretical ACF for the A1(1) process
Q4.MA1.acf<-ARMAacf(ar=c(0),ma=c(0.8),lag.max=100)
#Add a line (in red) that shows the theoretical ACF against lags
#0 to 100
x_lag<-c(0:100)
lines(x_lag,Q4.MA1.acf,col="red")


# Q5(a) -------------------------------------------------------------------

eng_rain <- read.csv("~/Documents/GitHub/Statistics/TimeSeries/Computer 1/eng_rain.csv",header=TRUE)

# Q5(b) -------------------------------------------------------------------

annual_rain<-ts(eng_rain$annual_rainfall_mm,start=1862,end=2021)

# Q5(c) -------------------------------------------------------------------

ts.plot(annual_rain,ylab="Annual rainfall (mm)",xlab="Year",
        main="Time plot of annual rainfall (mm)\n in England (1862-2021)")

acf(annual_rain,main="Sample ACF, England Rainfall Data")
