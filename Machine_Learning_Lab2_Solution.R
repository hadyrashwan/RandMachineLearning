################### Machine learning lab 2, Fisher disciminitive ########
#### Created by Hatem El-Shazly as A Teaching assistant in the introduction to machine learing course 

########Training ########

train_data = as.matrix(read.table("Data1.txt")) #read the data file and put it in matrix

labels = train_data[,3] #The third column in the data file is the labels (called t in slides)

X = train_data[, 1:2] #Seperate data from labels
X = t(X)

c1 = X[, labels==1] #Get points belonging to first class
c2 = X[, labels==-1] #Get points belonging to second class

m1 = rowMeans(c1)
m2 = rowMeans(c2)

Sw = matrix(0, nrow(X), nrow(X)) #Slide 20 in Lec2s

#Note there are shorter way to do it without for loop, but this is the most straight-forward
for(i in 1:ncol(c1)){
  temp = c1[, i] - m1
  Sw = Sw + temp %*% t(temp)
}

for(i in 1:ncol(c2)){
  temp = c2[,i] - m2
  Sw = Sw + temp %*% t(temp)
}

W = solve(Sw) %*% (m2-m1) #from slide 21 in Lec2

w1 = W[1]
w2 = W[2]

w0 = -t(W) %*% (m2+m1)/2

#transpose X to make the graph similar to Lab1
plot(t(X), col = ifelse(labels == 1, "yellow", "blue"))

abline(-w0/w2, -w1/w2) #Decision boundary, explained in: https://canvas.instructure.com/courses/1012948/discussion_topics/4610421

########### Testing ############

test_data = as.matrix(read.table("Test1.txt")) #read the test data and put it in matrix

X_test = cbind(test_data, 1) #Add columns with 1s to be multiplied by w0
W = rbind(W, w0) #concatenate w0 to W

y = t(W) %*% t(X_test)

## if y>0 then the point belong to class 1, else it belong to class -1
plot(test_data, col=ifelse(y>0, "red", "black"))

abline(-w0/w2, -w1/w2) #Decision boundry, explained in: https://canvas.instructure.com/courses/1012948/discussion_topics/4610421


