library(leaps)

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

getCandidateModel <- function(subsets, data, tolerance=1.2) {
  # Findet ein Kandidatenmodell mit einer minimalen Parameterzahl,
  # dessen Cp-Wert sich dem p-Wert von oben nähert
  pValue <- length(data)
  subsetPredictors = summary(subsets)$which
  cpValues = summary(subsets)$cp

  bestIndex <- 0
  for(i in seq(1, pValue)) {
    currentCp <- cpValues[i]
    if (currentCp >= pValue && currentCp <= pValue * tolerance) {
      bestIndex  <- i
      break
    }
  }
  if (bestIndex == pValue) {
    warning("No suitable model within the given Cp range found.")
  }
  cat("Using candidate model with", bestIndex, "predictors, Cp:", cpValues[bestIndex], "\n")

  includedPredictors <- subsetPredictors[bestIndex,]
  # -1: Prädiktoren ohne Intercept
  formula <- reformulate(names(which(includedPredictors[-1])), "N", TRUE)
  return(lm(formula, data))
}

nir.data <- read.csv("NIR.csv", sep = ";", header=TRUE)
nir.reducedData <- cbind(
                         nir.data[, "N", drop=FALSE],
                         repeatedSelectFeatures(nir.data[, 4:ncol(nir.data), drop=FALSE])
)
nir.subsets <- regsubsets(
  y=nir.reducedData[,"N"],
  x=nir.reducedData[,-1],
  nvmax=ncol(nir.reducedData[-1])+1,
  method="backward",
  really.big = TRUE)

nir.model <- getCandidateModel(nir.subsets, nir.reducedData)
