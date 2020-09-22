

### Explanation SCALE FUNCTION ###

# The scale function does a z-transform of the data, which means it takes the original values, 
# subtracts the mean, and then divides by the standard deviation.

to_scale <- 1:10
using_scale <- scale(to_scale, center = TRUE, scale = TRUE)
by_hand <- (to_scale - mean(to_scale))/sd(to_scale)
identical(as.numeric(using_scale), by_hand)


# Therefore, to reverse the model coefficients all you need to do is multiply the coefficient by the standard deviation 
# of the covariate and add the mean. The scale function holds onto the mean and sd for you. 
# So, if we assume that your covariate values are the using_scale vector for the scale.t6 regression coefficient we can write a function 
# to do the work for us.

get_real <- function(coef, scaled_covariate){
  
  # collect mean and standard deviation from scaled covariate
  mean_sd <- unlist(attributes(scaled_covariate)[-1])
  
  # reverse the z-transformation
  answer <- (coef * mean_sd[2]) + mean_sd[1]
  
  # this value will have a name, remove it
  names(answer) <- NULL
  
  # return unscaled coef
  return(answer)
}

get_real(-0.3440, using_scale)


