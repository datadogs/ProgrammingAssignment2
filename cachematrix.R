# This is Assignment 2 to create two functions used to cache the
# inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseM <<- inverse
  getinverse <- function() inverseM
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function computes the inverse of the special "matrix" returned by  
# makeCacheMatrix  above. If the inverse has already been calculated 
# (and the matrix has not changed), then  cacheSolve  should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
  inverseM <- x$getinverse()
  if(!is.null(inverseM)) {
    message("getting cached data.")
    return(inverseM)
  }
  data <- x$get()
  inverseM <- solve(data)
  x$setinverse(inverseM)
  inverseM
}

