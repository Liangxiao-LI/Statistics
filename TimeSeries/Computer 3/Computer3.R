
# Q1(a) -------------------------------------------------------------------

Nile

# Q1(b) -------------------------------------------------------------------


#Code to produce a time plot
ts.plot(Nile,xlab="Year",ylab="Annual flow")
#Code to produce a plot of the sample ACF against the lag
acf(Nile)
#Code to produce a plot of the sample PACF against the lag
pacf(Nile)


# Q1(c) -------------------------------------------------------------------

Nile_diff<-diff(Nile)

#Code to produce a time plot
ts.plot(Nile_diff,xlab="Year",ylab="Annual flow")
#Code to produce a plot of the sample ACF against the lag
acf(Nile_diff)
#Code to produce a plot of the sample PACF against the lag
pacf(Nile_diff)


# Q1(d) -------------------------------------------------------------------

#Code to fit an ARIMA model to the Nile dataset
#Here p = order of the AR part of the model
#q = order of the MA part of the model
#d = order of differencing
#p, d and q should be replaced by integers of your choice model1<-arima(Nile,order=c(p,d,q),method="ML")
#For example, to fit an ARMA(1,1) model we'd run the command
model1<-arima(Nile,order=c(1,0,1),method="ML")
#To view the parameter estimates, standard error estimates and
#other model features, run the command
model1

model.AR1<-arima(Nile,order=c(1,0,0),method="ML")

model.AR1
# mu_hat = 919.5, phi_hat = 0.51, sigma_hat^2 = 21225


# Q1(e) -------------------------------------------------------------------

resid.AR1<-residuals(model.AR1)

ts.plot(resid.AR1)
acf(resid.AR1)


# Q1(f) -------------------------------------------------------------------

LB_test<-function(resid,max.k,p,q){
  lb_result<-list()
  df<-list()
  p_value<-list()
  for(i in (p+q+1):max.k){
    lb_result[[i]]<-Box.test(resid,lag=i,type=c("Ljung-Box"),fitdf=(p+q))
    df[[i]]<-lb_result[[i]]$parameter
    p_value[[i]]<-lb_result[[i]]$p.value
  }
  df<-as.vector(unlist(df))
  p_value<-as.vector(unlist(p_value))
  test_output<-data.frame(df,p_value)
  names(test_output)<-c("deg_freedom","LB_p_value")
  return(test_output)
}

#Since p+q=1, we run the following command to perform the first ten
#Ljung-Box tests for the model residuals (max.k=11)
AR1.LB<-LB_test(resid.AR1,max.k=11,p=1,q=0)
#To see the table of P-values, type
AR1.LB
#To produce a plot of the P-values against the degrees of freedom and
#add a blue dashed line at 0.05, we run the commands
plot(AR1.LB$deg_freedom,AR1.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
abline(h=0.05,col="blue",lty=2)


# Q1(g) -------------------------------------------------------------------


