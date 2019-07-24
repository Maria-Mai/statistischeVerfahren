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

reducedFeatures <- selectFeatures(nir.data[,4:ncol(nir.data), drop=FALSE]) 
for (i in 1:5) {
  reducedFeatures <- selectFeatures(reducedFeatures)  
}

nir.model <- regsubsets(
  y=nir.data[,2],
  x=reducedFeatures,
  nvmax=ncol(reducedFeatures)+1,
  method="backward",
  really.big = TRUE)

subsetForModel <- which.min(summary(nir.model)$cp)
subsetM <- summary(nir.model)$which

sum(subsetM[32,])


lm1 <- lm(y=nir.data[,2], x)