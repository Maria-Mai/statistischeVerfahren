library(leaps)

nir.data <- read.csv("NIR.csv", sep = ";", header=TRUE)
head(nir.data)
ncol(nir.data)

is.data.frame(nir.data)

#nir.model <- regsubsets(y=nir.data[,2], x=nir.data[,4:ncol(nir.data)], really.big = TRUE)

selectFeatures <- function(m){ #m matrix mit allein einflussgroessen
  resultDF <- data.frame(matrix(ncol=0, nrow=nrow(m)))
  for(i in seq(1, ncol(m), 2)){ # Spaltenindex
    if (i >= ncol(m)) {
      resultDF <- cbind(resultDF, m[,i,drop=FALSE])
      return(resultDF)
    }
    
    difference <- abs(sum((m[i] - m[i+1])) / nrow(m))
    
    if(difference >= 0.001){
      resultDF <- cbind(resultDF, m[,i:(i+1)])
      
    }
    else{
      resultDF <- cbind(resultDF, m[,i,drop=FALSE])
    }
  }
  return(resultDF)
}

help <- selectFeatures(nir.data[,4:ncol(nir.data), drop=FALSE])
ncol(help)
head(help)
help2 <- selectFeatures(help)
ncol(help2)
help3 <- selectFeatures(help2)
ncol(help3)
help4 <- selectFeatures(help3)
ncol(help4)
help5 <- selectFeatures(help4)
ncol(help5)
help6 <- selectFeatures(help5)
ncol(help6)
help7 <- selectFeatures(help6)
ncol(help7)
help8 <- selectFeatures(help7)
ncol(help8)


nir.model <- regsubsets(y=nir.data[,2], x=help8, nvmax=ncol(help8)+1, method="backward", really.big = TRUE)
subsetForModel <- which.min(summary(nir.model)$cp)
subsetM <- summary(nir.model)$which

sum(subsetM[32,])

plot(nir.model, scale="r2")


lm1 <- lm(y=nir.data[,2], x)
  
  
  
#second step

variabilityWithMean <- function(m2){
  resultMatrix <- matrix(nrow=nrow(m2), ncol=0)
  for(i in 1:ncol(m2)){
    innerDifference <- (max(m2[,i]) - min(m2[,i])) / mean(m2[,i])
    if(innerDifference > 0.001){
      resultMatrix <- cbind(resultMatrix, m2[,i])
    }
  }
  return(resultMatrix)
}

variability <- function(m2){
  resultMatrix <- matrix(nrow=nrow(m2), ncol=0)
  for(i in 1:ncol(m2)){
    innerDifference <- max(m2[,i]) - min(m2[,i])
    if(innerDifference > 0.001){
      resultMatrix <- cbind(resultMatrix, m2[,i])
    }
  }
  return(resultMatrix)
}

help9 <- variabilityWithMean(help8)
ncol(help9)
help10 <- variabilityWithMean(help9)
ncol(help10)

help9 <- variability(help8)
ncol(help9)
help10 <- variability(help9)
ncol(help10)


