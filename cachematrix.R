## Put comments here that give an overall description of what your
## functions do

##inside of the first function, we have 2 variables.
##the matrix of the first function is assigned to mtrx, and cch caches the computed inverse.
##with the functions set and get, the mtrx updates and we can have the matrix.
##setcache updates the cch and getcache give us the chached inverse.
##cacheSolve funcrtion computes the inverse of mtrx. mtrx needs to be its argument as
##it was defined in the first function.
##mtrx is checked to not be null by mtrx$getcache
##if else loop is used in cacheSolve.if the cache inverse in not null, it will return cache inverse and
##prints "using cached inverse". if it is null, it retireves the current matrix and inverse the matrix
##using "solve" and caches the result by "mtrx$setcache(inverse). we can modify the code
##in a way that it returns null when cache inverse is available,
##but I don't think it is functional here(I am not sure because it disables the caching inverse)
## it is a function creating a matrix assigned to x

makeCacheMatrix <- function(x = matrix()) {mtrx <- x
cch <- NULL
list(
  set = function(y) {
    mtrx <<- y
    cch <<- NULL},
  get = function() mtrx,
  setcache = function(inverse) cch <<- inverse,
  getcache = function() cch)}


## Write a short comment describing this function
## the cacheSolve function takes one mandatory argument which is x which is the matrix.
##it can take optional arguments because we have "..."
##it returns the computed inverse of the matrix x.

cacheSolve <- function(x, ...) {cachedinversed <- mtrx$getcache()

if (!is.null(cachedinversed)) {
  cat("Using cached inverse/n")
  return(cachedinversed)
} else {
  cat("Computing and caching the inverse/n")
  
  x <- mtrx$get()
  inverse <- solve(x, ...)
  mtrx$setcache(inverse)
  return(inverse)}
      }
