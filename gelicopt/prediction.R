
# The following functions smooth the observed learning curves by getting the median over a small moving window 
filter <- function(vec, window){
  sapply(1:length(vec), function(n){median(vec[max(1, (n + 1 - window)) : n])})
}

filterDataset <- function(dataset, window = 5){
  res <- lapply(split(dataset, dataset$id), function(x){
    x$validation <- filter(x$validation, window)
    x$training <- filter(x$training, window)
    
    x
  })
  
  do.call("rbind", res)
}


# We use a simple linear model to predict guessability error
# datasets: a vector of filenames to be used as training data 
buildModel <- function(datasets, filter = FALSE) {
  dataset.list <- lapply(datasets, read.csv)
  trainData <- do.call(rbind, dataset.list)
  
  if(filter) { # Optionally, I could first filter
    trainData <- filterDataset(trainData)
  }
  
  trainData$error <- trainData$population - trainData$sample
  trainData$diff <- trainData$training - trainData$validation
  
  lmodel <- lm(error ~ diff + N, data = trainData) 
  
  lmodel
}


# Predict the guessability error. The function expects that testData is a data frame with columns: N, training, validation 
predictError <- function(model, testData, filter = FALSE){
  testData$id <- 1
  
  if(filter){ # I filter again the dataset to smooth out differences (optional)
    testData <- filterDataset(testData) 
  }
  
  testData$diff <- testData$training - testData$validation
  
  predict(model, newdata = data.frame(diff = testData$diff, N = testData$N), interval = "prediction")
}



