## This program uses a special data structure to store the inverse 
## of a matrix which avoids repeated calculations


## makeCacheMatrix() accepts a matrix and return a list 
## representation of this matrix that has 4 child-functions:
## set(), get(), setinverse(), and getinverse()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() {
    inv
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() accepts a list representation of a matrix
## and calculates or searchs for the cached inverse of it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}