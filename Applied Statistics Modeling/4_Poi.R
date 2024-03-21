age=rep(c(40,50,60,70,80),2)
smoke=c(rep(1,5),rep(2,5))
deaths=c(32,104,206,186,102,2,2,28,28,31)
years=c(52407, 43248,28612,12663,5317,18790,10673,5710,2585,1462)
smokedata=cbind(age,smoke,deaths,years)
smokedata

smokef=factor(smoke)
Lage=log(age)
Lyears=log(years)

out=glm(deaths ~ smokef + Lage, family=poisson, offset=Lyears)
summary(out)
