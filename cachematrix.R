## This script caches the inverse of a matrix so that it can be
## called from memory without needing to re-run the computation.
## If the matrix inverse does not yet exist, it is calculated.

## makeCacheMatrix() creates an object to store a matrix and the
## inverse of that matrix. Functions are created then called in
## a list to:
## 1) set the matrix,
## 2) get the matrix,
## 3) calculate the inverse of the matrix, and
## 4) get that inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve() checks to see if there is an existing inverted
## matrix before commiting to computing the inverse to avoid
## wasting time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("Retrieving cached inverse matrix.")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  return(i)
}
