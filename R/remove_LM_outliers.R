## function to look for outliers of linear regression ------------
remove_LM_outliers <- function(model) {

  # check for outliers
  df <- model$model
  ei.s <- residuals(model)/sqrt(sum(residuals(model)^2)/(nrow(df) - length(model$coefficients)))
  alpha <- 0.1 ; n = nrow(df); p = length(model$coefficients)
  cutoff <- qt(1-alpha/(2*n), n -p )
  cutoff.deleted <- qt(1-alpha/(2*n), n -p -1 )
  outliers <- which(abs(ei.s) > cutoff)

  # create new data without the outliers

  # return new data
  if (length(outliers) == 0) {
    return("No outliers detected.")
  } else{
    new.data <- df[-outliers,]
    return(new.data)
  }

}




