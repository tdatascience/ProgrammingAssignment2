## makeCacheMatrix and cacheSolve are a pair of functions allowing the user to
## cache the possible expensive result of a matrix inversion. This should save on
## CPU time if this value is needed multiple times.

## makeCacheMatrix function creates an R object that stores a matrix object
## and provides functions for acccessing the stored matrix (get), storing the matrix
## inverse (setinv), and retrieving the matrix inverse(getinv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve - this function partners with the objects returned by makeCacheMatrix
## It retrieves the inverse of the matrix, using a cached value if it has alreday
## been run, or by computing the inverse.

cacheSolve <- function(x) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
