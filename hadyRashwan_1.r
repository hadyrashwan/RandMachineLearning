#
#import ds1 & ds2
ds1=EuStockMarkets[,"DAX"]
ds2=EuStockMarkets[,"SMI"]
#########
#mean1
prev= 0
for(x in ds1){sum1<- sum1 +abs(x- prev)
prev=x

 }
mean1=sum1/length(ds1)
#Absloute change = value of indicator2- value of indicator 1 
#--
#variance1
prev = 0
for(x in ds1){ sumforVariance1 = (abs(x- prev)-mean1)* (abs(x- prev)-mean1)
prev=x

}
remove(sumforVariance)
variance1=sumforVariance1/length(ds1)



#mean2
prev = 0
sum2=0
for(x in ds2){sum2<- sum2 +abs(x - prev)
prev=x
}
mean2=sum2/length(ds2)

#variance2
prev = 0
for(x in ds2){ sumforVariance2 = (abs(x- prev)-mean2)   * (abs(x- prev)-mean2)
	prev=x


}
variance2=sumforVariance2/length(ds2)

#mean DS1 U DS2
d1Ud2=union(ds1,ds2)
d1Ud2Sum=0
for(x in d1Ud2){d1Ud2Sum<- d1Ud2Sum +x}
d1Ud2Mean=d1Ud2Sum/length(d1Ud2)
########


#variance DS1 U DS2
for(x in d1Ud2){ sumforVarianced1Ud2 = (x-d1Ud2Mean)* (x-d1Ud2Mean)}
variancesD1UD2=sumforVarianced1Ud2/length(d1Ud2)





#Kurtosis

for(x in ds1){sumforKurtosis1=(((x-mean1)* (x-mean1)*(x-mean1)*(x-mean1))/(variance1*variance1))}
len1=length(ds1)
kurtosis1=sumforKurtosis1*((len1*(len1+1))/((len1-1)*(len1-2)*(len1-3)))

#skewness
for(x in ds1){sumforskewness1=(((x-mean1)* (x-mean1)*(x-mean1))/(variance1))}
skewness1=sumforskewness1*((len1*(len1+1))/((len1-1)*(len1-2)))

#ploting
boxplot(d1Ud2)
hist(d1Ud2)

