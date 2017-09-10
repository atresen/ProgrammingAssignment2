## This set of functions can be used to cache a matrix in the global environment. It can then
## solve the inverse of the matrix and cache that new matrix. After the inverse matrix has been
## cached, if solveCache is called for a second time on the same matrix, the cached inverse matrix
## is returned instead of performing the calculations.

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)

## This function is a list of functions that provides the means to cache matrices and their inverses
## in the global environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  invmx <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get=get, invmx=invmx, getinv=getinv)
}

cm <- makeCacheMatrix(x)

## cacheSolve will compute the inverse matrix of x only if inv is NULL. Otherwise it will find
## the cached version of inv and return that.

cacheSolve <- function(x=matrix(), ...) {
  inv <- cm$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- cm$get()
  inv <- solve(x)
  cm$invmx(inv)
  inv
}

## Return a matrix that is the inverse of 'x'
