## These 2 functions work together to store the inverse of a matrix
## with the matrix so that it only needs to be calculated one time

## The first function creates an object - a list - that contains 
## four functions and sets values for the matrix, x, as well as
## the inverse of that matrix, inv, which is initially set to NULL.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setSolve <- function(solve) inv <<- solve
  getSolve <- function() inv
  list(set = set, 
       get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)
}

## The second function reads the object created by the first function
## and either calculates the inverse, or retrieves the inverse 
## from the environment of getSolve(), if inv was calculated before

cacheSolve <- function(x, ...) {
  inv <- x$getSolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- round(solve(data, ...),digits=5)
  x$setSolve(inv)
  inv
}