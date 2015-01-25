## The following are two sepparate functions that cache 
## and compute the inverse of a matrix

## The first function, 'makeCacheMatrix' creates a special
## "matrix" object that can cache its inverse

makeCacheMatrix5 <- function(mtx = matrix()) {
      inverse <- NULL
      set <- function(x) {
            mtx <<- x
            inverse <<- NULL
      }
      get <- function() return(mtx)
      setinv <- function(inv) inverse <<- inv
      getinv <- function() return(inverse)
      return(list(set = set, get = get,
                  setinv = setinv, 
                  getinv = getinv))
}

## This second function, "cacheSolve" computes the 
## inverse of the special "matrix" returned by 
## 'makeCacheMatrix' above. If the inverse has 
## already been calculated (and the matrix 
## has not changed), then "cacheSOlve" should 
## retrieve the inverse from the cache. 

cacheSolve5 <- function(mtx, ...) {
      inverse <- mtx$getinv()
      if(!is.null(inverse)) {
            message("Getting cached data...")
            return(inverse)
      }
      data <- mtx$get()
      inverse <- solve(data, ...)
      mtx$setinv(inverse)
      return(inverse)
}
