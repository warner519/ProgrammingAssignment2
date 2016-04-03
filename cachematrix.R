## a matrix

## makeCacheMatrix() creates a special matrix object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix = NULL
  set = function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get = function() x
  setInvMatrix = function(invertMatrix) invMatrix <<- invertMatrix
  getInvMatrix = function() invMatrix
  list(set=set, get=get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}


## cacheSolve() computes the inverse of the matrix returned
## by makeCacheMatrix() or, if the inverse has already been
## calculated and has not changed, it retrieves the inverse
## from the cache directly

cacheSolve <- function(x, ...) {
  invMatrix = x$getInvMatrix()
  if (!is.null(invMatrix)) {
    message("Getting Cached Data")
    return(invMatrix)
  }
  mat.data = x$get()
  invMatrix = solve(mat.data, ...)
  x$setInvMatrix(invMatrix)
  return(invMatrix)
}
