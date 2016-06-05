#import Data
library(jpeg)
library(pracma)
setwd("/home/hady/Downloads/Machine Learning - Assignment 2/Problem 2 Dataset/Noise Train")
listFiles = list.files()
imageMatrixTest = matrix(1,ncol=144, nrow = 182)


i=1 #counter
for(file in listFiles)
{
  imageMatrixTest[i, 1:144] = as.vector(readJPEG(file))
  i = i+1
}
#Make a Matrix that contians the distance between each 2 images 182x182 Matrix
allDistanceMatrix= matrix(0,ncol=182, nrow = 182)
for(i in 1:182) # where all the distances will be saved
{
	for(j in i:182)#Continue after the first loop index
	{
		distance = dist(rbind(imageMatrixTest[i,1:144],imageMatrixTest[j,1:144]))#A function that gets the distance
		allDistanceMatrix[j,i]=distance
		allDistanceMatrix[i,j]=distance 
	}

}
error = matrix(0,ncol=100,nrow=1) 
for(tryNumber in 1:10){
  print(tryNumber)
	  randomNumber = sample(seq(1,182,1)) #Genreate random number
	  indexRandomTrainTest = ceiling(randomNumber/7)#get the class and avoid the float numbers using ceiling
	  distanceTrainVector = matrix(0,ncol=146,nrow=1) # A vector for 146 elements for training 
	  for(testPoints in 147:182){## in every testing itra make all the 146 for training
	    for ( trainPoints in 1:146) 
		{

		      distanceTrainVector[1,trainPoints] = allDistanceMatrix[randomNumber[trainPoints],randomNumber[testPoints]]		## fill the vector with distances of the 2 taken images which are made randomlly 

		}
	distanceTrainVectorSorted = sort.int(distanceTrainVector, index.return=TRUE) # sort the distances
 	sortedTrainIndex = matrix(0,ncol=146,nrow = 1)
   	 for (sortedTrainPoints in 1:146) {
    		  sortedTrainIndex[sortedTrainPoints] = indexRandomTrainTest[distanceTrainVectorSorted$ix[sortedTrainPoints]] 
    		  ## get the index of the smallest distance and then put in the random sample then put it in the sorted vector 
   	 }

	for(k in 1:100)
	{
	      afterSorting = sort(table(sortedTrainIndex[1,1:k]),decreasing=TRUE) # get the length up to k  and then use the table function after that apply sort
	      if (!is.na(as.vector(afterSorting)[2])){ #if the length bigger than 2 for excpetion handling
	        if (as.vector(afterSorting)[1] > as.vector(afterSorting)[2]){
	          if(indexRandomTrainTest[testPoints] != attributes(afterSorting)[[2]][[1]][1]){
              		  error[1,k] =  error[1,k]+1   } ## inc that K error by one
		  
	           }
	        else{
		        afterSorting = sort(table(sortedTrainIndex[1,1:1]),decreasing=TRUE)
          		if(indexRandomTrainTest[testPoints] != attributes(afterSorting)[[1]][[1]][1]){
          		  error[1,k] =  error[1,k]+1 }         
		          }
        	}
	else{ ## will be used when only one value also a exception handling condition
       		 afterSorting = sort(table(sortedTrainIndex[1,1:1]),decreasing=TRUE)
       		 if(indexRandomTrainTest[testPoints] != attributes(afterSorting)[[1]][[1]][1]){
       		   error[1,k] =  error[1,k]+1  }
      }     
	  }
 	}
}
K = which.min(error) 
## the K that will be used all the training phase above was to get this 
## however from here u can use the built in function of knn  in R or repeat the above process 
######Testing using the Noise Test folder        
setwd("/home/hady/Downloads/Machine Learning - Assignment 2/Problem 2 Dataset/Noise Test")
files = list.files()
testData = matrix(1,ncol=144, nrow = 182)

i=1
for(file in files)
{
  testData[i, 1:144] = as.vector(readJPEG(file))
  i = i+1
}



classes=c(rep(0,52))
indexTesting=ceiling(seq(1,182,1)/7)
for (testPoints in 1:52) {
  distanceTest = matrix(0,ncol=182,nrow=1) 
  for (trainPoints in 1:182) {
    distanceTest[1,trainPoints] = dist(rbind(testData[testPoints,1:144],imageMatrixTest[trainPoints,1:144]))### here fix 
  }
  sortdistanceTest = sort.int(distanceTest, index.return=TRUE)
  afterSortingIndex = matrix(0,ncol=182,nrow = 1)
  for (sortTestPoints in 1:182) {
    afterSortingIndex[sortTestPoints] = indexTesting[sortdistanceTest$ix[sortTestPoints]]
  }
  afterSorting = sort(table(afterSortingIndex[1,1:K]),decreasing=TRUE)
  if (!is.na(as.vector(afterSorting)[2])){
    if (as.vector(afterSorting)[1] > as.vector(afterSorting)[2]){
      if(ceiling(testPoints/2) == attributes(afterSorting)[[2]][[1]][1]){
        classes[testPoints] = ceiling(testPoints/2)
      }
    }
    else
    {
      afterSorting = sort(table(afterSortingIndex[1,1:1]),decreasing=TRUE)
      if(ceiling(testPoints/2) == attributes(afterSorting)[[1]][[1]][1]){
        classes[testPoints] = ceiling(testPoints/2)
      }
    }
  }
  else
  {
    afterSorting = sort(table(afterSortingIndex[1,1:1]),decreasing=TRUE)
    if(ceiling(testPoints/2) == attributes(afterSorting)[[1]][[1]][1]){
      classes[testPoints] = ceiling(testPoints/2)
    }
  }    
}
print("for some reason I Cant draw so I printed the vector")
print(classes)
