#Create dataset
shipment=factor(rep(1:5,each=3))
carrier=factor(rep(letters[1:3],times=5))
times=c(15.2,16.9,17.1,
        14.3,16.4,16.1,
        14.7,15.9,15.7,
        15.1,16.7,17.0,
        14.0,15.6,15.5)
Delivery=data.frame(times,shipment,carrier)
Delivery

#Explore
boxplot(times~shipment,data=Delivery)
boxplot(times~carrier,data=Delivery)

#Fitting a CRD (No shipment effect)

mod.crd=aov(times~carrier,data=Delivery)
## summary of overall test of significance
summary(mod.crd)
## since crd is a special case of lm,
## we can use summary.lm to obtain coefficient info
summary.lm(mod.crd)

plot(mod.crd,which=2)

#Fitting an RCBD

mod.rcbd=aov(times~carrier+shipment,data=Delivery)
summary(mod.rcbd)

summary.lm(mod.rcbd)
