## Two functions that will cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  
  set <- function( a ) {
    l <<- a
    k <<- NULL
  }
  
  get <- function(){
    l
  }
  
  setInverse <- function(inverse) {
    k <<- inverse
  }
  
  getInverse <- function() {
    k
  }
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  l <- x$getInverse()
  
  if( !is.null(l) ) {
    message("getting cached data")
    return(l)
  }
  
  data <- x$get()
  l <- solve(data) %*% data
  x$setInverse(l)
  l
}