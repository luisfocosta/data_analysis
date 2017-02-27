## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly
# This function creates a special "matrix" object that can cache its inverse.
# Note: assume that the matrix supplied is always invertible


makeCacheMatrix <- function(x = matrix()) {
   inv_matrix <- NULL
   set <- function(y) {
      x <<- y
      inv_matrix <<- NULL
   }
   get <- function () x
   setinv <- function(inverse) inv_matrix <<- inverse
   getinv <- function () inv_matrix
   list (set=set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatri.
#If the inverse has already been calculated (and the matrix has not changed), then the
#cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinv()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinv(m)
   m
}
