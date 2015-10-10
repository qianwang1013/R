##############################################
############### IRIS

iris_myC45 <- function(){
  library(datasets)
  data("iris")
  result <- myC45(Species~., iris)

}

iris_myC45Predict <- function(){
  fit <- iris_myC45()
  library(datasets)
  data("iris")
  myC45Predict(fit, iris[,1:4], iris$Species)

}

##############################################
###############Life Expectancy

LE_myC45 <- function(){
  LE <- getLE()
  result <- myC45(LE$Continent~., LE)
  result

}

LE_myC45Predict <- function(){
  fit <- LE_myC45()
  LE <- getLE()
  myC45Predict(fit, LE[,1:7], LE$Continent)

}

getLE <- function(){
  LE <- read.csv(file = "life_expectancy.csv", header = TRUE, sep = ",")
  LE
}

############################################
myC45 <- function(subset, dataset){
  #install.packages("RWeka", repos="https://cran.r-project.org/")
  library(RWeka)
  fit <- J48(subset, dataset)
}

myC45Predict <- function(fit, sample, Sample){
  library(RWeka)
  predictions <- predict(fit, sample)
  table(predictions, Sample)
}
##########################################


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
