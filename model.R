
# Importing libraries
library(RCurl) # for downloading the iris CSV file
library(randomForest)
library(caret)

# Importing the Iris data set
iris <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/iris.csv") )

iris$Species <- factor(iris$Species)

TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "C:/Viraj Personal Data/Code/R Shiny/training.csv")
write.csv(TestingSet, "C:/Viraj Personal Data/Code/R Shiny/testing.csv")

TrainSet <- read.csv("C:/Viraj Personal Data/Code/R Shiny/training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

###
#The Species column is set as characters:
str(TrainSet)
#It has to be changed to factors to avoid the non-numeric argument error:
TrainSet$Species = as.factor(TrainSet$Species)

str(TrainSet)

# Building Random forest model

model <- randomForest(Species ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)

# Save model to RDS file
saveRDS(model, "C:/Viraj Personal Data/Code/R Shiny/model.rds")
