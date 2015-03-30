# 0-setup.r
# Time-stamp: c:/x/stat/bayesiansm/0-setup.r

# Display summary of mcmc.list more concisely
lucid.mcmc.list <- function(x, dig=4, ...) {
  s <- summary(x)
  out <- data.frame(s$statistics[,"Mean"],
                    s$statistics[,"SD"],
                    s$quantiles[,1],
                    s$quantiles[,3],
                    s$quantiles[,5])
  colnames(out) <- c('Mean','SD','2.5%','Median','97.5%')
  lucid(out, dig=dig)        
}
