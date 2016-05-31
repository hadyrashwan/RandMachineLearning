######## Training ########

## Created by Hatem El-Shazly as A Teaching assistant in the introduction to machine learing course 

trainData = as.matrix(read.table("Data1.txt"))#read the data file and put it in matrix



labels = trainData[,3] #The third column in the data file is the labels (called t in slides)



X = cbind(trainData[,1:2],1) #Replace the labels column with 1s



W = solve(t(X) %*% X) %*% t(X) %*% labels # Least squares solution equation from slide 13 in Lecture 2



plot(X[,1:2], col = ifelse(labels==1,'red','green')) #Plot the training points, each class has one color



w1 = W[1] #See slide 12 in Lecture 2

w2 = W[2] #See slide 12 in Lecture 2

w0 = W[3] #See slide 12 in Lecture 2



abline(-w0/w2, -w1/w2) #Decision boundry, explained in: https://canvas.instructure.com/courses/1012948/discussion_topics/4610421


######## Testing ########


testData = as.matrix(read.table("Test1.txt"))#read the test data and put it in matrix



X_Test = cbind(testData,1) #Add columns with 1s to be multiplied by w0



y = t(W) %*% t(X_Test) # if y>0 then the point belong to class 1, else it belong to class -1



plot(X_Test, col = ifelse(y>0,'red','green')) #Plot the test points and color them by detected classes



abline(-w0/w2, -w1/w2) #Decision boundry, explained in: https://canvas.instructure.com/courses/1012948/discussion_topics/4610421
