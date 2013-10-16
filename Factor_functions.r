PCA_factors <- function(data, percentage)
{
#Percentage is the amount of variance that we wish to be explained
#by the selected factors

EigenMat = eigen(cor(dataset))
TotVar = sum(EigenMat$values)
ProgVar = cumsum(EigenMat$values)

nbFactors = which(ProgVar >= percentage*TotVar)[1]

MainFactors = EigenMat$vectors[,1:nbFactors]

return(MainFactors)
}
