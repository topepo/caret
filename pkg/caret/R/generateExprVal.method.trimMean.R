"generateExprVal.method.trimMean" <-
function(probes, trim = 0.15) 
{
   mu <- apply(probes, 2, mean, trim = trim)
   s <- rep(NA, length(mu))
   return(list(exprs = mu, se.exprs = s))
}
