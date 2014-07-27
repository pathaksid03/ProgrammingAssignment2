## This function creates a matrix object that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  m <- NULL
  ## Setting the matrix
  set <- function( matrix ) {
    x <<- matrix
    m <<- NULL
  }
  ## Getting the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    m <<- inverse
  }
   ## To get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    m
  }
  
  ## This returns a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function computes the inverse of the matrix returned by the above function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## If the inverse is already cached, this fetches the cached data and returns inverse.
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## If not, the matrix is ccached in following manner
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m<-solve(matrix, ...)
   ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
