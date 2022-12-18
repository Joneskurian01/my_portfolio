res4=c()
for (trial in 1:40){
  time=c(0)
  interarrivals=c()
  for (i in 1:5000){
    interarrival=rexp(1,1/2)
    time=c(time,time[i]+interarrival)
    interarrivals=c(interarrivals,interarrival)
  }
  time=time[2:5001]
  time.sample=runif(700,0,8.5)
  time.sample=sort(time.sample)
  time.sample=exp(time.sample)
  time.sample=2000+time.sample
  indexes=c()
  ii=1
  for (i in time.sample){
    while (time[ii]<=i) {
      ii=ii+1
    }
    indexes=c(indexes,ii)
  }
  res1=interarrivals[indexes]
  res4=c(res4,res1)
}
hist(res2,freq=F,breaks=25)#unif
hist(res3,freq =F,breaks = 25)#logarithmic 1
hist(res4,freq =F,breaks = 25)#logarithmic 2
x=seq(0,15,0.1)
lines(x,dgamma(x,2,1/2),col='red')
hist(interarrivals,freq = F,breaks=35)
lines(x,dexp(x,1/2),col='blue')
'the reason why the interarrival times at randomly selected large values
t has a different distribution than the tor. Is because if you randomly 
select a point in time to visit, you are disproportionately likely to get 
a time where the interarrival is larger thus creating a weighted distribution.
But it remains the case that looking forward the at T(nt) the time till 
T(nt+1) is still t.

I just checked the picking of a random time does not need to be uniform, so
long as you r picking a long time after the initiation of a trial you 
will see this size biased dist. res2 was attained by randomly selecting times
between 2000 & 9000 while res3 was attained through logarithmic selection of 
time between 2000 & 9000.'

j=1;prop=matrix(0,nrow = 10000,4)
while(j<10000){
    i=2
    y=sample(c(0,1,2,3),400,replace=T,prob=c(0.2,0.2,0.3,0.3))
    x=c(0)
  while(x[length(x)]!=3){
    if (y[i]>x[i-1]){
      x[i]=y[i]
      prop[j,y[i]]=i-sum(prop[j,(1:(y[i]-1))])
    }
    else{
      x[i]=x[i-1]
    }
    i=i+1
  }
  prop[j,4]=400-sum(prop[j,1:3])
  print(prop[j,1:4])
  j=j+1
}
y
for (i in 1:4){
  print(sum(prop[,i])/(400*10000))
}
