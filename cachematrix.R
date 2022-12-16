#makeCacheMatrix Function

## Cache function to cache the inverse of the matrix
## Usually generating single inverse of a matrix may result in faster computational time.
## Larger datasets involving large number of inverse calculations however will be slow.
## The functions written below creates a matrix and caches its inverse.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve Function

## This function takes the matrix generated above 
## and calculates its inverse if not calculated earlier 
## behaving like a cache memory of the inverse matrix.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
