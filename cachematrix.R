## Create special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ## create a special matrix object that can cache its inverse
    m <- NULL     ## initialize the inverse property
    set <- function(y) {     ## method to set the matrix
      x <<- y
      m <<- NULL
    }
    get <- function() x    ## method to get the matrix
    setinversematrix <- function(inverse) m <<- inverse   ## method to set the inverse of matrix
    getinversematrix <- function() m   ## method to get the inverse of matrix
    list(set = set, get = get,        ## return a list of matrix
          setinversematrix = setinversematrix,
          getinversematrix = getinversematrix)
}


## Compute the inverse of the special matrix returned by 'makeCacheMatrix'.
## If the inverse has already been calculated (and the matrix has not change
## the 'cacheSolve' should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()   ## return a matrix that is the inverse of 'x'
  if(!is.null(m)) {           ## return the inverse if it is already set
          message("getting cached data")
          return (m)
  }
  data <- x$get()              ## get the matrix from our object
  m <- solve(data, ...) %*% data  ## get the inverse using matrix multiplication
  x$setinversematrix(m)           ## set the inverse to the object
  m                               ## Return a matrix that is the inverse of 'x'
}
