##Caching an Inverse of a Matrix Using R 
##There are functions that will  get the inverse of the matrix by not doing any repitition of code and will produce the output.
##In Java programming a function to cache the matrix and the inverse is similar to getter and setter .
##Converting rows of a matrix into columns and columns of a matrix into row is called transpose of a matrix.

## This assignment will create two functions  that work together to create a square invertible matrix
##and make the inverse of the matrix available in the cache environment.

## 1. makeCacheMatrix creates and returns a list of functions used by cacheSolve to get or set the 
##inverted matrix in cache.

## 2.cacheSolve calculates the inverse of the matrix created in makeCacheMatrix. If the inverted 
##matrix does not exist in cache,it it created in the working environment and it's inverted value
#is stored in cache.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}