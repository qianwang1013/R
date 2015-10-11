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

LE_Ripper <- function(){
  LE <- getLE()
  myRipper(Continent~., LE)
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
iris_Predict <- function(fit){
  library(datasets)
  data("iris")
  myC45Predict(fit, iris[,1:4], iris$Species)
}

myC45Predict <- function(fit, sample, Sample){
  library(RWeka)
  predictions <- predict(fit, sample)
  table(predictions, Sample)
}

##### Ripper and its prediction
myRipper <- function(data, class){
  library(RWeka)
  JRip(data, class);

}
LE_Predict <- function(fit){
  LE <- getLE()
  myC45Predict(fit, LE[,1:7], LE$Continent)

}

getLE <- function(){
  LE <- read.csv(file = "life_expectancy.csv", header = TRUE, sep = ",")
  LE
}





##########################################
#########################################
######### Ripper ########################





#####################################


#####################################

# Reading datasets and dividing into training and test sets
divideDataset <- function(ori_set){
  # returns:
  # dataset$training
  # and dataset$test
  set.seed(6991) #69538991
  training<- iris[sample(nrow(iris),size=0.8*nrow(iris),replace=FALSE),]
  test <- iris[sample(nrow(iris),size=0.2*nrow(iris),replace=FALSE),]
  dataset <- list( training  = training,test = test)
  dataset
}

