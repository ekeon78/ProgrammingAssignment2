## Caching the inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  
  ##set the value of the inverse
  setinverse <- function(solve) m <<- solve
  
  ##get the value of the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not been changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Check if inverse has been calculated. If so, it gets the inverse from cache and skips the computation
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##Otherwise, calculate the inverse of the data and sets the value of the inverse in the cache via the setinverse function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
       
}
