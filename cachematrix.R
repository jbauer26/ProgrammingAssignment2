## functions for Programming Assignment 2 (Lexical Scoping)

## this function is used to create an object that is associated to a matrix and
## is able to cache the inverse of that matrix 
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## this function returns the inverse of a given "CacheMatrix"-object
## if the inverse of the given object is already cached, the cached matrix is 
## returned

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  # there is no specific if-case needed to check if the matrix has changed 
  # because when the matrix is changed, i is set to NULL 
  if(!is.null(i)) {
    print("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  
}
