
# converts a data frame into a “LongitudinalData” object
make_LD <- function(x){
  structure(x, class = "LongitudinalData")
}

# a generic function for extracting subject-specific information
subject <- function(x, i) UseMethod("subject")

subject.LongitudinalData <- function(x, i) {
  
  index <- which(x$id %in% i)
  x <- lapply(x, function(x) x[index])
  
  structure(x, class = "LongitudinalData")
}

# a generic function for extracting visit-specific information
visit <- function(i, j) UseMethod("visit")

visit.LongitudinalData <- function(x, j) {
  
  index <- which(x$visit %in% j)
  x <- lapply(x, function(x) x[index])
  
  structure(x, class = "LongitudinalData")
  
}

# a generic function for extracting room-specific information
room <- function(i, j) UseMethod("room")

room.LongitudinalData <- function(x, k) {
  
  index <- which(x$room == k)
  x <- lapply(x, function(x) x[index])
  
  structure(x, class = "LongitudinalData")
  
}

# print method for “LongitudinalData” object
print.LongitudinalData <- function(x, ...) {
  
  if(length(unique(x$id)) == 1){
    cat("Subject ID: ", unique(x$id), "\n")  
  } else if(length(unique(x$id)) == 0) {
    cat("NULL")
  } else {
    cat("Longitudinal dataset with", length(unique(x$id)), "subjects")
  }
  if(length(unique(x$visit)) == 1){
    cat("Visit: ", unique(x$visit), "\n")
  }
  if(length(unique(x$room)) == 1){
    cat("Room: ", unique(x$room))
  }
  invisible(x)
}

# summary function
summary.LongitudinalData <- function(object, ...) {
  object <- list(
    summary.id = unique(object$id),
    summary.data = data.frame(
      visit = object$visit,
      room = object$room,
      value = object$value
    ) 
    )
  if(length(unique(object$summary.data$visit)) == 1 & 
     length(unique(object$summary.data$room)) == 1){
    
    object$summary.data <- summary(object$summary.data$value)
    
  } else {
    
    object$summary.data <- object$summary.data %>% 
      aggregate(value ~ visit + room, FUN = mean, data = .) %>%
      spread(room, value)
    
  }
     
  class(object) <- "summary_LongitudinalData"
  object
}

# print summary method for “LongitudinalData” object
print.summary_LongitudinalData <- function(x, ...) {
  cat("Subject ID: ", x$summary.id, "\n")
  print(x$summary.data)
  invisible(x)
}


