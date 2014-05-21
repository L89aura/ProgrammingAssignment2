## Functions are supposed to create a cache matrix object. I should calculate the inverse only
##once
##  cacheMatrix$set(M)      # Change the matrix being cached.
##  M <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x
## Matrix inversion is  a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than computing it repeatedly


## First function: Create a special matrix  object than can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
      x <<- y
      cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
}



 ## Second Function: Returns a matrix that is the inverse of the special matrix returned from function one. If the inverse has already
 ##been calculated then cachSolve should retrieve the inverse from the cache.
}
cacheSolve <- function(x, ...) {
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}


