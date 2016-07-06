## Import data
setwd("/home/hady/Downloads/Machine Learning - Assignment 3")
data = as.matrix(read.table("SpectData.txt"))

size = length(data[,1])
sigma1 = 0.01
sigma2 = 0.05
sigma3 = 0.1
##cal 
spectralCluestering=function(sigma,dataSize){
  
  Weight = matrix(0, nrow = dataSize, ncol = dataSize)
  WeightSums = matrix(0, nrow = dataSize , ncol = dataSize)
  Diffrence = matrix(0, nrow = dataSize, ncol = dataSize)
  
  for(i in 1:dataSize)
  {
    for(j in 1:dataSize)
    {
      Weight[i,j] = exp(  -1* ( (data[i,1]-data[j,1])**2 + (data[i,2]-data[j,2])**2 ) / (2*sigma*sigma)  ) ## the eqution as given
    }
    WeightSums[i,i] = sum(Weight[i,]) ## Sum of every row to create a refrence matrix
    print(i) ## see which itr has been made
  }
  Diffrence = WeightSums - Weight ## diff
  
  afterEigen = eigen(Diffrence)
  
 return( afterEigen$vector[, order(afterEigen$values)==2] )
  
}

plot(data, col= ifelse(spectralCluestering(sigma1,size)<0,"red", "blue"),main = "0.01") 
plot(data, col= ifelse(spectralCluestering(sigma2,size)<0,"red", "blue"), main = "0.05") 
plot(data, col= ifelse(spectralCluestering(sigma3,size)<0,"red", "blue"), main = "0.1)") 
