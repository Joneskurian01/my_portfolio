client=read.csv("C:/Users/qazwa/Downloads/archive/BankChurners.csv")
head(client)
client=client[,1:21]
client[,2]=gsub('Existing Customer',0,client[,2])
client[,2][client[,2]!='0']=1
client[,2]=as.numeric(client[,2])
model1=glm(cbind(client$Attrition_Flag,1-client$Attrition_Flag)~.-CLIENTNUM,family=binomial,data=client)
summary(model1)
model2=step(model1)
summary(model2)
pchisq(4719,10103)
client$Card_Category
summary(model2)
'data have different ranges so a large coefficient for a value that tends to be 
infintesimal might not be as important as a small coefficient for a category which
tends to have large values. We need to take a sample of values and find the average
contribution that each category makes to the probability of attrition to find the 
significant factors.'
ndata=client %>% rename(transamt=Total_Trans_Amt,transct=Total_Trans_Ct,attrition=
                          Attrition_Flag)
ndata=ndata[,2:21]
#there could be interaction between transamt and tranct
model4=glm(cbind(ndata$attrition,1-ndata$attrition)~.+transamt*transct,family = binomial,ndata)
model4=step(model4)#get rid of irrelevant variables
anova(model3,model4,test = 'Chisq')
#the model with interaction is much better
summary(model4)
sum(resid(model4,'pearson')^2)/10102
#the model becomes slightly overdispersed but it still very good
summary(lm(model4$y~model4$fitted.values))
1-sum((model4$y-model4$fitted.values)^2)/sum((model4$y-mean(model4$y))^2)
coeff=model4$coefficients
plot(ndata$transct,ndata$transct*coeff[23]+ndata$ctchng*coeff[24]+ndata$ctchng*ndata$transct*coeff[25],col='red',ylab = 'ETA')
plot(ndata$ctchng,ndata$transct*coeff[23]+ndata$ctchng*coeff[24]+ndata$ctchng*ndata$transct*coeff[25],col='red',ylab = 'ETA')
#increases in ctchng decrease the eta,increaseas in transct decreases eta.
effect=c()
effect[2]=(mean(ndata$dependant)*coeff[])^2+var(ndata$dependant)*coeff[4]^2
effect[3]=(mean(ndata$relationshipcount)*coeff[16])^2+var(ndata$relationshipcount)*coeff[16]^2
effect[4]=(mean(ndata$inactivity)*coeff[17])^2+var(ndata$inactivity)*coeff[17]^2
effect[5]=(mean(ndata$contactsc)*coeff[18])^2+var(ndata$contactsc)*coeff[18]^2
effect[6]=(mean(ndata$creditlimit)*coeff[19])^2+var(ndata$creditlimit)*coeff[19]^2
effect[7]=(mean(ndata$revolvingbal)*coeff[20])^2+var(ndata$revolvingbal)*coeff[20]^2
effect[8]=(mean(ndata$amtchng)*coeff[21])^2+var(ndata$amtchng)*coeff[21]^2
effect[9]=(mean(ndata$transamt)*coeff[22]+mean(ndata$transamt*ndata$transct)*coeff[25])^2+
  var(ndata$transamt)*coeff[22]^2+var(ndata$transamt*ndata$transct)*coeff[25]^2
effect[10]=(mean(ndata$transct)*coeff[23]+mean(ndata$transamt*ndata$transct)*coeff[25])^2+
      var(ndata$transamt*ndata$transct)*coeff[25]^2+var(ndata$transct)*coeff[23]^2
effect[11]=(mean(ndata$ctchng)*coeff[24])^2+var(ndata$ctchng)*coeff[24]^2
effect=sqrt(effect)
effect
#now i need the calculate the sqaured difference for transamt & transct so given the two are 
# highly correlated I will use a model connecting the 2
coeff
plot(ndata$transamt~ndata$transct,col='blue')
sq=ndata$transct^2
summary(lm(ndata$transamt~ndata$transct+sq))
sum(as.numeric((ndata$transct>70)&(ndata$transamt<=2700)))
mean(predict(model4,ndata[(ndata$transct>70)&(ndata$transamt<=2700),]))
#-6.868
mean(predict(model4,ndata[(ndata$transct<=60)&(ndata$transamt>=4500),]))
#2.3866
VAL=function(x,y){
  return(coeff[22]*x+coeff[23]*y+coeff[25]*x*y)
}
#investigation of transamt &transct
qmod=lm(ndata$transamt~ndata$transct+sq)
qmod
x=as.list(distinct(ndata,transct))
x=x$transct
length(x)
x=arrange(x)
sq=x^2
head(sq)
res=coeff[18]*x+coeff[17]*predict(qmod,data.frame(x,sq))+coeff[20]*x*predict(qmod,data.frame(x,sq))
plot(res~ndata$transct)
#the effect is strictly decreasing so more transaction and therefore more trans
#volume will reduce the attrition rate but once the transct passes a certain 
#number the attrition drops fast
res=data.frame(x=ndata$transct,y=res)
res=arrange(res,x)
head(res)
plot(predict(qmod,data.frame(ndata$transct,transct2))~ndata$transct,col='red')
points(ndata$transct,ndata$transamt,col='blue')
fcoeff

