#1 Dice 1000 thorws
Dice1in1000=sample(1:6, 1000, replace = T)
hist(Dice1in1000 , breaks=100)

#2 dices 1000 throws
Dices2in1000=sample(1:6,2,replace=T)
Dices2in1000=mean(Dices2in1000)
for(i in 2:1000){
Dices2=sample(1:6,2,replace=T)
mean(Dices2)
Dices2in1000<-c(Dices2in1000,Dices2)
}
hist(Dices2in1000 , breaks=100)
mean(Dices2in1000)
var(Dices2in1000)

#10 dices 1000 throws
Dices10in1000=sample(1:6,10,replace=T)
Dices10in1000=mean(Dices10)
for(i in 2:1000){
Dices10=sample(1:6,10,replace=T)
mean(Dices10)
Dices10in1000<-c(Dices10in1000,Dices10)
}
var(Dices10in1000)
mean(Dices10in1000)
hist(Dices10in1000 , breaks=100)



#the more dies we add the more the output is noramlly distributed
savehistory("~/Downloads/ass2.r")
