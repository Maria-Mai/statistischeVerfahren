rss <- function(model)
    sum(residuals(model)^2)

sigma2Tilde <- function(model, modelSize, sampleSize)
    rss(model) / (sampleSize - modelSize)

trueSPSE <- function(model, modelSize, sampleSize)
    (sampleSize + modelSize) * sigma2Tilde(model, modelSize, sampleSize)

trueSPSE2 <- function(model, modelSize, fullModel, sampleSize, fullModelSize)
    rss(model) + 2 * sigma2Tilde(fullModel, fullModelSize, sampleSize) * modelSize

estimatedSPSE <- function(cp, fullModel, fullModelSize, sampleSize)
    cp * sigma_2_tilde_full + sampleSize * sigma2Tilde(fullModel, fullModelSize, sampleSize)
