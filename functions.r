##############################################
############### IRIS User Interact Functions
### Find the fit for test set
iris_myC45 <- function(){
  library(datasets)
  data("iris")
  dataset <- divideDataset(iris)
  result <- myC45(Species~., dataset$training)
  result
}

iris_myC45Predict<-function(){
  fit <- iris_myC45()
  library(datasets)
  data("iris")
  dataset <- divideDataset(iris)
  myC45Predict(fit, dataset$test[,1:4], dataset$test$Species)
}


iris_Ripper <- function(){
  library(datasets)
  data("iris")
  dataset <- divideDataset(iris)
  fit <- myRipper(Species~., dataset$training)
  fit
}
iris_RipperPredict <- function(){
  fit <- iris_Ripper()
  library(datasets)
  data("iris")
  dataset <- divideDataset(iris)
  myC45Predict(fit, dataset$test[,1:4], dataset$test$Species)
}

iris_myOblique <- function(){
  library(datasets)
  data("iris")
  dataset <- divideDataset(iris)
  myOblique(Species~., dataset$training)
}

iris_myObliquePredict <- function(){
  fit <- iris_myOblique()
  library(datasets)
  data("iris")
  dataset <- divideDataset(iris)
  myC45Predict(fit, dataset$test[,1:4], dataset$test$Species)
}

##############################################
############### Life Expectancy User Interact Functions


LE_myC45 <- function(){
  LE <- getLE()
  dataset <- divideDataset(LE)
  result <- myC45(dataset$training$Continent~., dataset$training)
  result

}

LE_myC45Predict <-function(){
  ####Calling middile group function passing correct fit

  LE_Predict(LE_myC45())
}

LE_Ripper <- function(){
  LE <- getLE()
  dataset <- divideDataset(LE)
  result <- myRipper(dataset$training$Continent~., dataset$training)
  result
}

LE_RipperPredict <- function(){
  ####Calling middile group function passing correct fit

  LE_Predict(LE_Ripper())
}

LE_myOblique <- function(){
  LE <- getLE()
  dataset <- divideDataset(LE)
  result <- myOblique(dataset$training$Continent~., dataset$training[,3:8])
  result
}

LE_myObliquePredict <- function(){
  LE <- getLE()
  dataset <- divideDataset(LE)
  myC45Predict(LE_myOblique(), dataset$test[,3:7], dataset$test$Continent)
}

LE_Predict <- function(fit){
  LE <- getLE()
  dataset <- divideDataset(LE)
  myC45Predict(fit, dataset$test[,1:7], dataset$test$Continent)

}

############################################
#### Base Methods #########################
##########################################



##### C45 and its prediction
myC45 <- function(subset, dataset){
  #install.packages("RWeka", repos="https://cran.r-project.org/")
  library(RWeka)
  J48(subset, dataset)
}


myC45Predict <- function(fit, sample, Sample){
  library(RWeka)
  predictions <- predict(fit, sample, type = "class")
  table(predictions, Sample)
}

##### Ripper and its prediction
myRipper <- function(data, class){
  library(RWeka)
  JRip(data, class)
}

##### oblique

myOblique <- function(data, class){
  #install.packages("oblique.tree", repos="https://cran.r-project.org/")
  library("oblique.tree")
  tree(data, class)
}


getLE <- function(){
  LE <- read.csv(file = "life_expectancy.csv", header = TRUE, sep = ",")
  LE
}



#####################################

# Reading datasets and dividing into training and test sets
divideDataset <- function(ori_set){
  # returns:
  # dataset$training
  # and dataset$test
  set.seed(6991) #69538991
  training<- ori_set[sample(nrow(ori_set),size=0.8*nrow(ori_set),replace=FALSE),]
  test <- ori_set[sample(nrow(ori_set),size=0.2*nrow(ori_set),replace=FALSE),]
  dataset <- list( training  = training,test = test)
  dataset
}


#####################################
iris_naiveBayes <- function(){
  library(datasets)
  data("iris")
  dataset <- divideDataset(iris)
  nb(dataset$training[,1:4], dataset$training$Species)
}

iris_nbPredict <- function(){
  fit <- iris_naiveBayes()
  library(datasets)
  data("iris")
  dataset <- divideDataset(iris)
  myC45Predict(fit, dataset$test[,1:4], dataset$test$Species)
}

LE_naiveBayes <- function(){
  LE <- getLE()
  dataset <- divideDataset(LE)
  nb(dataset$training[,1:7], dataset$training$Continent)
}

LE_nbPredict <- function(){
  fit <- LE_naiveBayes()
  LE <- getLE()
  dataset <- divideDataset(LE)
  myC45Predict(fit, dataset$test[1:7], dataset$test$Continent)
}

nb <- function(data, class){
  library("e1071")
  naiveBayes(data, class)

}

######################################

iris_myKnn <- function(){
  library(datasets)
  data("iris")
  dataset <- divideDataset(iris)

  library("class")
  result <- knn(dataset$training[,1:4], dataset$test[,1:4], dataset$training[,5], 1)
  summary(result)
}

LE_myKnn <- function(){
  LE <- getLE()
  LE_dataset <- divideDataset(LE)

  library("class")
  result <- knn(LE_dataset$training[,3:7], LE_dataset$test[,3:7], LE_dataset$training[,8], 1)
  summary(result)
}
