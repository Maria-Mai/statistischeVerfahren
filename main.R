library(leaps)

nir.data <- read.csv("NIR.csv", sep = ";", header=TRUE)
head(nir.data)
ncol(nir.data)

#nir.model <- regsubsets(y=nir.data[,2], x=nir.data[,4:ncol(nir.data)], really.big = TRUE)

selectFeatures <- function(m){ #m matrix mit allein einflussgroessen
  resultList <- matrix(nrow=nrow(m), ncol=0)
  for(i in seq(1, ncol(m), 2)){ # Spaltenindex
    print(i)
    
    if (i >= ncol(m)) {
      resultList <- cbind(resultList, m[,i])
      return(resultList)
    }
    
    difference <- abs(sum((m[i] - m[i+1])) / nrow(m))
    print(difference)
    
    if(difference >= 0.001){
      resultList <- cbind(resultList, m[,i:i+1])
    }
    else{
      resultList <- cbind(resultList, m[,i])
    }

  }
  return(resultList)
}

help <- selectFeatures(nir.data[,4:ncol(nir.data)])
ncol(help)
