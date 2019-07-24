library(leaps)

nir.data <- read.csv("NIR.csv", sep = ";", header=TRUE)

selectFeatures <- function(df){ #df Data Frame mit allen Einflussgroessen
  resultDF <- data.frame(matrix(ncol=0, nrow=nrow(df)))
  for(i in seq(1, ncol(df), 2)){ # Spaltenindex
    if (i >= ncol(df)) {
      resultDF <- cbind(resultDF, df[,i,drop=FALSE])
      return(resultDF)
    }
    
    difference <- abs(sum((df[i] - df[i+1])) / nrow(df))
    
    if(difference >= 0.001){
      resultDF <- cbind(resultDF, df[,i:(i+1)])
      
    }
    else{
      resultDF <- cbind(resultDF, df[,i,drop=FALSE])
    }
  }
  return(resultDF)
}

repeatedSelectFeatures <- function(df, maxCount=10) {
  reducedFeatures <- selectFeatures(df)
  for (i in 1:maxCount) {
    newFeatures <- selectFeatures(reducedFeatures)
    if (ncol(newFeatures) == ncol(reducedFeatures)) {
      return (reducedFeatures)
    }
    reducedFeatures <- newFeatures
  }
  return(reducedFeatures)
}

getBestLinearModel <- function(subsets, data) {
  subsetWhich = summary(subsets)$which
  bestId <- which.min(summary(subsets)$cp)
  varNames <- dimnames(subsetWhich)[[2]][-1]
  bestModel <- subsetWhich[bestId,]
  formula <- reformulate(varNames[which(bestModel[-1])], "N", bestModel[1])
  return(lm(formula, data))
}

reducedFeatures <- repeatedSelectFeatures(nir.data[,4:ncol(nir.data), drop=FALSE])
nir.subsets <- regsubsets(
  y=nir.data[,2],
  x=reducedFeatures,
  nvmax=ncol(reducedFeatures)+1,
  method="backward",
  really.big = TRUE)

nir.model <- getBestLinearModel(nir.subsets, nir.data)