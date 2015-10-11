##############################################
############### IRIS User Interact Functions

iris_myC45 <- function(){
  library(datasets)
  data("iris")
  result <- myC45(Species~., iris)

}

iris_myC45Predict<-function(){
  iris_Predict(iris_myC45())
}

iris_Ripper <- function(){
  library(datasets)
  data("iris")
  myRipper(Species~., iris)
}
iris_RipperPredict <- function(){
  ####Calling middile group function passing correct fit
  iris_Predict(iris_Ripper())
}

iris_Predict <- function(fit){
  library(datasets)
  data("iris")
  myC45Predict(fit, iris[,1:4], iris$Species)
}

iris_myOblique <- function(){
  library(datasets)
  data("iris")
  myOblique(Species~., iris)
}

iris_myObliquePredict <- function(){
  ####Calling middile group function passing correct fit
  iris_Predict(iris_myOblique())
}

##############################################
############### Life Expectancy User Interact Functions


LE_myC45 <- function(){
  LE <- getLE()
  result <- myC45(LE$Continent~., LE)
  result

}

LE_myC45Predict <-function(){
  ####Calling middile group function passing correct fit

  LE_Predict(LE_myC45())
}

LE_RipperPredict <- function(){
  ####Calling middile group function passing correct fit

  LE_Predict(LE_Ripper())
}
LE_Predict <- function(fit){
  LE <- getLE()
  myC45Predict(fit, LE[,1:7], LE$Continent)

}
LE_Ripper <- function(){
  LE <- getLE()
  myRipper(Continent~., LE)
}

LE_myOblique <- function(){
  LE <- getLE()
  myOblique(Continent~., LE[3:8])
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
  predictions <- predict(fit, sample)
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
  nb(iris[,1:4], iris$Species)
}

LE_naiveBayes <- function(){
  LE <- getLE()
  nb(LE[,1:7], LE$Continent)
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
