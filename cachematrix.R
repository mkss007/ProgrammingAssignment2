## Programming Assignment 2
## x is a matrix passed so that it can be inverted
## Variable inverse is set to NULL to keep track if the matrix in this has already been inverted
## If "inverse" is NULL that means matrix has been set but not inverted
## If variable "inverse" is NOT NULL that means matrix is inverted and cached already

## This function caches the inverted matrix so that it does not have to be recomputed

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Sets up the matrix and inverse variable so that they are available in the other environment
  # The other environment is what is available to the cacheSolve function
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Retrieves the matrix
  get <- function() {
    x
  }
  
  # Sets the inverse variable and makes it available in the other environment
  # The other environment is what is available to the cacheSolve function
  setinv <- function(inv) {
    inverse <<- inv   
  }
  
  # Retrieves the inverse variable
  getinv <- function() {
    inverse
  }
  
  # Return this list to the consuming function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##  Calculate inverse by calling this routine, if not null get cached value 
##  otherwise get the matrix and find inverse, then set and return the inverse
##  This is the consuming function so it uses the list returned by the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matUncached <- x$get()
  if (nrow(matUncached) != ncol(matUncached)) {
    message("for this exercise only square matrices are expected - try again")
  } else {
    inverse <- solve(matUncached)
    x$setinv(inverse)
    return(inverse)
  }
}
