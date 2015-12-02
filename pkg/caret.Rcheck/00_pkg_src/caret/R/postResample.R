postResample <- function(pred, obs)
{

  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]

  if(!is.factor(obs) & is.numeric(obs))
    {
      if(length(obs) + length(pred) == 0)
        {
          out <- rep(NA, 2)
        } else {
          if(length(unique(pred)) < 2 || length(unique(obs)) < 2)
            {
              resamplCor <- NA
            } else {
              resamplCor <- try(cor(pred, obs, use = "pairwise.complete.obs"), silent = TRUE)
              if(class(resamplCor) == "try-error") resamplCor <- NA 
            }
          mse <- mean((pred - obs)^2)
          n <- length(obs)

          out <- c(sqrt(mse), resamplCor^2)
        }
      names(out) <- c("RMSE", "Rsquared")    
    } else {
      if(length(obs) + length(pred) == 0)
        {
          out <- rep(NA, 2)
        } else {
          pred <- factor(pred, levels = levels(obs))  
          requireNamespaceQuietStop("e1071")
          out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag", "kappa")]
        }
      names(out) <- c("Accuracy", "Kappa")         
    }
  if(any(is.nan(out))) out[is.nan(out)] <- NA
  out
}
