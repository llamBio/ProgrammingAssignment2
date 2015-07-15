## cachematrix.R
#  create a matrix that can cache its inverse value

## makeCacheMatrix
#  This function creates a special "matrix" object that can cache its inverse.
#  Args:
#    x: numeric matrix that is invertible.
makeCacheMatrix <- function(x = matrix()) {
  inv.mat = NULL  ## initialize inverse matrix
  set <- function(y) {  ## initialize the matrix values
    x <<- y
    inv.mat <<- NULL
  }
  get <- function() x  ## returns the numeric matrix
  setinvmat <- function(inv.mat.m) inv.mat <<- inv.mat.m    ## sets inv mat
  getinvmat <- function() inv.mat   ## gets inv mat
  list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
}

## cacheSolve()
# This function returns the inverse matrix using the cache of the 
#   special matrix if available.
# Args:
#   x: Special matrix object.
# Returns:
#   a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv = x$getinvmat()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvmat(inv)
  inv
}
