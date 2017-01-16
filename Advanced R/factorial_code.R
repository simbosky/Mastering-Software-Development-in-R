# 1. Factorial_loop: a version that computes the factorial of an integer using 
# looping (such as a for loop)

Factorial_loop <- function(x){
  
  # ensure x is an integer
  if(!is.integer(x)){
    stop("x must be an integer; to ensure use 'L' (i.e. x = 4L)", call. = FALSE)
  }
  
  if(x == 0){
    return(1)
  } else{
    y <- 1
    for(i in 1:x){
      y <- y * ((1:x)[i])
    }
    return(y)
  }
}

# 2. Factorial_reduce: a version that computes the factorial using the reduce() 
# function in the purrr package. Alternatively, you can use the Reduce() function 
# in the base package.

Factorial_reduce <- function(x){
  
  # ensure x is an integer
  if(!is.integer(x)){
    stop("x must be an integer; to ensure use 'L' (i.e. x = 4L)", call. = FALSE)
  }
  
  # ensure purrr package is installed
  if (!require('purrr', quietly = TRUE)) {
    stop('Please install the purrr package')
  }
  
  if(x == 0){
    return(1)
  } else{
    reduce(as.numeric(1:x), `*`) %>% return()
    }
}

# 3. Factorial_func: a version that uses recursion to compute the factorial.

Factorial_func <- function(x){
  
}


