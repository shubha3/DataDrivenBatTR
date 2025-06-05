# This is a function which will import all the functionalities of
# the packages listed in the vector entered by a user


# Install and load packages
import_lib <- function(pack){
  lapply(pack, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  })
}

# NA utils
check_na <- function(df, operator = "sum"){
  # Checking if the operator is a valid function
  if( !exists(operator, mode = "function")) {
    stop(paste("Invalid operator:", operator, "is not a recognized function"))
  }
  
  result_na <- apply(df, 2, is.na)
  
  # converting operator to a function
  func <- match.fun(operator)
  
  if (operator == "sum"){
    message("Calculating the total number of missing values for each column")
    result <- result_na %>% apply(., 2, sum)
  } else if (operator == "mean"){
    message("Calculating the proportion of missing values in each column")
    result <- result_na %>% apply(., 2, mean)
  } else {
    message(paste("Applying a custom operator:", operator))
    
    result <- result_na %>% 
      summarise(across(everything(), func))
  }
  
  return(result)
}

# This function adaptively selects an observation around which we should focus for
# thermal runaway study
# parameters:
# 
# data: battery data with appropriate format
# variable: the variable based on which the window of interest needs to be adjusted; default: "temperature"
# threshold: the value controlling the level of sensitivity to extreme observation; default: 1.2
sig_shift_window <- function(data,
                             variable = "temperature",
                             threshold = 1.2,
                             left.size = 200,
                             right.size = 100
                             ){
  k <- 1
  baseline <- 0; max_next <- 0
  while(abs(max_next) <= threshold*abs(baseline)){
    baseline <- mean(data[1:k, variable])
    max_next <- max(c(baseline, data[k+1, variable]))
    k <- k + 1
  }
  return(list("X.range" = c(k - left.size, k,  k + right.size)))
}

sig_shift_mod_u <- function(data,
                            variable = "temperature",
                            threshold = 1.12,
                            left.size = 100,
                            right.size = 100,
                            burnin = 5000){
  k <- 5000
  baseline <- mean(data[1:k, variable]); max_next <- 0
  
  while(abs(max_next) <= threshold*abs(baseline)){
    
    baseline <- mean(data[1:(k+1), variable])
    max_next <- max(c(baseline, data[k+2, variable]))
    k <- k + 1
  }
  
  return(list("X.range" = c(k - left.size, k,  k + right.size)))
}







