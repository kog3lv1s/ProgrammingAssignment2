## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix stores a matrix into cache, cacheSolve outputs
## the inverse matrix. if the inverse matrix is not already contained within
## cache then cacheSolve will also be responsible for solving the inverse

## Write a short comment describing this function
## makeCacheMatrix creates cache matrix and functions to be called in cacheSolve
## creates a variable m to store the inverse data in cache
## set assigns the new input and clears cache
## get allows cacheSolve to receive the input matrix
## setinv is responsible for solving the inverse matrix
## getinv is used for inputting inverse matrix
## list assigns names for aforementioned functions to be called in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m = NULL 
  set <- function(y) {
    x <<- y 
    m <<- NULL 
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## Write a short comment describing this function
## returns the inverse of matrix "x" which may or may not be stored in cache
## if the cache is storing the inverse matrix it will return its value
## if the cache is not storing the inverse matrix the inverse matrix will be
## calculated and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("pls wait, getting cache data")
    return(m)
  }
  cache <- x$get()
  m <- solve(cache, ...)
  x$setinv(m)
  m
}
