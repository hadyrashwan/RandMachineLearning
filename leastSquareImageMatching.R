
library(jpeg)
library(pracma)

##import data
##setwd("~/Downloads/Machine Learning - Assignment 1 (1)/Assignment 1 Dataset/Train")  ## Replace with ur path then uncomment 
files = list.files()
data = matrix(1,ncol=145, nrow = 182)

i=1 ##
for(file in files)
{
  data[i, 1:144] = as.vector(readJPEG(file))
  i = i+1
}
dataV2 = pinv(t(data) %*% data) %*% t(data)
## Cal Weight
wieghtCal = function(Index)
  {
  oneLetterMatrix = matrix(ncol=1, nrow = 182)
  for(i in 1:182)
    {
       if(i>=(Index-1)*7 + 1 && i<=(Index)*7)
          oneLetterMatrix[i,1] = 1
        else
          oneLetterMatrix[i,1] = -1
  }
  W = dataV2 %*% oneLetterMatrix
  returnValue(W)
}
weight=matrix(nrow=26, ncol=145)
for(i in 1:26)
{
  weight[i,] = wieghtCal(i)
}
##Testing
##setwd("/home/hady/Downloads/Machine Learning - Assignment 1 (1)/Assignment 1 Dataset/Test") ## Replace with ur path uncomment
filesTest = list.files()
dataTest = matrix(1,ncol=145, nrow = 182)

##import data[testing] 
i=1
for(file in filesTest)
{
  dataTest[i, 1:144] = as.vector(readJPEG(file))
  i = i+1
}
## test weights 
output=c(rep(0,52))
for(i in 1:52)
{
  minError=0
  letterIndex=0
  for(j in 1:26)
  {
    test=t(weight[j,])%*%dataTest[i,]
    if(test>minError)
    {
      minError=test
      letterIndex=j
    }
  }
  output[i]=letterIndex
}
##ploting
lettersRightValues = rep(c(1:26), each = 2)
lettersRight = rep(0,length(letters))
for(i in 1:52)
{
  if(output[i] == lettersRightValues[i])
  {
    lettersRight[lettersRightValues[i]] =  lettersRight[lettersRightValues[i]] + 1
  }
}
## Can't Plot but the in the output[] only the 12th letter was wrong
#plot(factor(letters), lettersRight) ## Gives an error
print(c(output))
print(c("Can't plot' figure margins too large error ' but only one char was wrong in my test"))



