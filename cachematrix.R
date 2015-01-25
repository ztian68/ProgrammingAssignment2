## Matrix inversion is usually a costly computation 
## These two functions are to caching the inverse of a matrix 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  IX <- NULL
  set <- function(y) {
    x <<- y
    IX <<- NULL
  }
  get <- function() x
  setInverX <- function(InverX) IX <<- InverX
  getInverX <- function() IX
  list(set = set, get = get,
       setInverX = setInverX,
       getInverX = getInverX)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  IX <- x$getInverX()
  if(!is.null(IX)) {
    message("getting cached data")
    return(IX)
  }
  data <- x$get()
  IX <- solve(data, ...)
  x$setInverX(IX)
  IX
        ## Return a matrix that is the inverse of 'x'
}
