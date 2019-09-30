library(leaps)
source("calculations.R")
source("simulations.R")

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

getCandidateModel <- function(subsets, data, tolerance=1.2, indexOnly=FALSE) {
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
  if (bestIndex + 1 == pValue) {
    warning("No suitable model within the given Cp range found.")
  }
  cat("Using candidate model with", bestIndex, "predictors, Cp:", cpValues[bestIndex], "\n")
  if (indexOnly) return(bestIndex)

  includedPredictors <- subsetPredictors[bestIndex,]
  # -1: Prädiktoren ohne Intercept
  formula <- reformulate(names(which(includedPredictors[-1])), "N", TRUE)
  return(lm(formula, data))
}

getCandidateModel2 <- function(subsets, data) {
  # Findet ein Kandidatenmodell mit minimalem Cp-Wert
  subsetPredictors = summary(subsets)$which
  cpValues = summary(subsets)$cp
  bestIndex = which.min(cpValues)

  cat("Using candidate model with", bestIndex, "predictors, Cp:", cpValues[bestIndex], "\n")
  cat("R2:", summary(subsets)$rsq[bestIndex], "\n")
  includedPredictors <- subsetPredictors[bestIndex,]
  formula <- reformulate(names(which(includedPredictors[-1])), "N", TRUE)
  return(lm(formula, data))
}

nir.data.raw <- read.csv("../NIR.csv", sep = ";", header=TRUE)
preprocessedFeatures <- scale(
                              repeatedSelectFeatures(nir.data.raw[, 4:ncol(nir.data.raw), drop=FALSE]),
                              center=TRUE, scale=TRUE)
nir.data.processed <- cbind(
                         nir.data.raw[, "N", drop=FALSE],
                         preprocessedFeatures
)
nir.subsets <- regsubsets(
  y=nir.data.processed[,"N"],
  x=nir.data.processed[,-1],
  nvmax=ncol(nir.data.processed[-1])+1,
  method="backward",
  really.big = TRUE)

#nir.model.candidate <- getCandidateModel(nir.subsets, nir.data.processed)
nir.model.candidate <- getCandidateModel2(nir.subsets, nir.data.processed)
nir.model.full <- lm(reformulate(names(nir.data.processed[-1]), "N"), nir.data.processed)

# Simulationen
SAMPLE_SIZES <- c(150, 200, 250, 300, 350, 400, 450, 500, 533)
N_REPETITIONS <- 10
for (j in SAMPLE_SIZES) {
	sum_cp <- 0
	sum_est_spse <- 0
	for (k in 1:N_REPETITIONS) {
		result <- simulation(nir.model.candidate, nir.data.processed, j)
		sum_cp <- sum_cp + result["cp"]
		sum_est_spse <- sum_est_spse + estimatedSPSE(result["cp"], nir.model.full, j)
	}
	cp_avg <- sum_cp / N_REPETITIONS
	est_spse_avg <- sum_est_spse / N_REPETITIONS
	cat("Sample Size:", j, "Average Cp:", cp_avg, "Average estimated SPSE:", est_spse_avg, "\n")
}
true_spse <- trueSPSE(nir.model.candidate, nir.model.full, nrow(nir.data.processed))
cat("True SPSE: ", true_spse, "\n")
#cat("True SPSE 2: ", trueSPSE2(nir.model.candidate, nir.model.full, 533), "\n")
