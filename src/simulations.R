library(leaps)

randomSample <- function(data, size)
	data[sample(nrow(data), size), ]

pseudoObservations <- function(model, data, repetitions) {
	results <- data.frame(matrix(nrow=nrow(data), ncol=repetitions))
	designMatrix <- model.matrix(model, data=data)
	predictions <- designMatrix %*% coef(model)
	residuals <- data$N - predictions
	sd <- sqrt(as.numeric(t(residuals) %*% residuals / (nrow(data) - ncol(designMatrix))))

	for (i in 1:repetitions) {
		results[i] = rnorm(nrow(data), mean=predictions, sd=sd)
	}

	return(rowMeans(results))
}

simulation <- function(model, data, sampleSize, repetitions=1000) {
	randSample <- randomSample(data, sampleSize)
	randSample$N <- pseudoObservations(model, randSample, repetitions)

	subsets <- regsubsets(
	  reformulate(names(data[-1]), "N"),
	  data=randSample,
	  nvmax=ncol(randSample[-1])+1,
	  method="backward",
	  really.big=TRUE)

	bestIndex <- which.min(summary(subsets)$cp)
	bestCp <- summary(subsets)$cp[bestIndex]
	bestR2 <- summary(subsets)$rsq[bestIndex]

	result <- c(bestIndex, bestCp, bestR2)
	names(result) <- c("index", "cp", "rsq")

	return(result)
}
