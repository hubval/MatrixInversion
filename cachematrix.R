## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. In R, we will use the solve function for matrix inversion.
## (Alternatives to matrix inversion will not be discussed here)



## makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse. We'll use anonymous function here for the additionnal attributes
## we want to add to our matrix.

## @Param : 'x' is a square invertible matrix
## Return a customized matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  cacheSolve function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

## @Param : 'x' is the customized matrix returned by previous function
## @Param : '...' for other argument that you want to use in solve function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  # Search cache inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If the inverse isn't in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
