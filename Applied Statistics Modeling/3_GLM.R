n=c(2,6,3,3,3,1,5,3,6,2)
y=c(1,2,1,1,0,1,3,2,3,1)
ag=c(rep(1,5),rep(2,5))
x=rep(c(500,5000,10000,40000,100000),2)

blooddata=cbind(n,y,ag,x)
blooddata

y2=cbind(y,n-y)
ag=factor(ag)

out1 = glm(y2 ~ ag + log(x), family=binomial(link="logit"))

summary(out1)

vcov(out1)
