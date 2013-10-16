PCA_factors <- function(data, percentage)
{
#Percentage is the amount of variance that the selected factors
#have to explain. use 0.5 for 50%

EigenMat = eigen(cor(dataset))
TotVar = sum(EigenMat$values)
ProgVar = cumsum(EigenMat$values)

nbFactors = which(ProgVar >= percentage*TotVar)[1]

MainFactors = EigenMat$vectors[,1:nbFactors]

return(MainFactors)
}
