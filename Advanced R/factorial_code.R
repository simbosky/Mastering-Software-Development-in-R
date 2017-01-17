# 1. Factorial_loop: a version that computes the factorial of an integer using 
# looping (such as a for loop)

Factorial_loop <- function(x){
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to, or greater than, zero")
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
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to, or greater than, zero")
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
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to, or greater than, zero")
  }
  
  if (x == 0){
    return (1)
  } else{
    return (x * Factorial_func(x-1))
  }          
}

# 4. Factorial_mem: a version that uses memoization to compute the factorial.

memoization <- function(){
  
  values <- 1
  
  Factorial_mem <- function(x){
    
    if(x < 0){
      stop("Factorials can only be computed when x is equal to, or greater than, zero")
    }
    
    if (x == 0 | x == 1){
      return(1)
    } 
    
    if (length(values) < x){
      values <<- `length<-`(values, x)
    }
    
    if (!is.na(values[x])){
      return(values[x])
    }
    
    #calculate new values
    values[x] <<- x * factorial(x-1)
    values[x]
  }
  Factorial_mem
}

Factorial_mem <- memoization()

# benchmarking these four functions
library(microbenchmark)
microbenchmark(
  Factorial_loop(1),
  Factorial_reduce(1),
  Factorial_func(1),
  Factorial_mem(1)
)

microbenchmark(
  Factorial_loop(10),
  Factorial_reduce(10),
  Factorial_func(10),
  Factorial_mem(10)
  )

microbenchmark(
  Factorial_loop(100),
  Factorial_reduce(100),
  Factorial_func(100),
  Factorial_mem(100)
)
