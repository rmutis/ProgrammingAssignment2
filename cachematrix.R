## The function makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse.
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse 
## 4.  get the value of the inverse  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
 

## The function cacheSolve returns a matrix that is the inverse of 'x'. 
## The if-loop checks if the inverse matrix has been already calculated and
## returns the result from the cache. If not it will be calculated and set.
  

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


