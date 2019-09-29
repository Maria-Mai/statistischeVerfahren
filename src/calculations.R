rss <- function(model)
    sum(residuals(model)^2)

sigma2Tilde <- function(model, sampleSize)
    rss(model) / (sampleSize - model$rank)

trueSPSE <- function(model, sampleSize)
    (sampleSize + model$rank) * sigma2Tilde(model, model$rank, sampleSize)

trueSPSE2 <- function(model, fullModel, sampleSize)
    rss(model) + 2 * sigma2Tilde(fullModel, sampleSize) * fullModel$rank

estimatedSPSE <- function(cp, fullModel, sampleSize)
    cp * sigma2Tilde(fullModel, sampleSize) + sampleSize * sigma2Tilde(fullModel, sampleSize)
