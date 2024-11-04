tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x))
  {
    outliers[,j] <- outliers[,j] & tukey.outlier(x[,j])
  }
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x))
  { 
    outlier.vec[i] <- all(outliers[i,]) 
  }
  return(outlier.vec) 
}




tukey.outlier <- function(x) {
  # 1st and 3rd quartiles
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  
  # interquartile range (difference)
  iqr <- q3 - q1
  
  # lower and upper bounds for outliers
  lower_bound <- q1 - 2.5 * iqr
  upper_bound <- q3 + 2.5 * iqr
  
  # Identify outliers
  return(x < lower_bound | x > upper_bound)
}


# Create a sample data frame
set.seed(11)
data <- data.frame(
  A = rnorm(8),
  B = rnorm(8),
  C = rnorm(8)
)

# Identify outliers
outliers <- tukey_multiple(data)
print(outliers)

