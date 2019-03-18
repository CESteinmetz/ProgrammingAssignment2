## Calculate Matrix Inverse if value not already cached. If cached, returns cached value

## Creates a vector that is a list of functions that get and set the values
## of the matrix, sets the cached inverse, and returns it
makeCacheMatrix <- function(x = matrix()) {
 
 inverse <- NULL
 set <- function(y) {
   x <<- y
   inverse <<- NULL
 }
 
 get <- function() x
 
 setinverse <- function(inverseVal) inverse <<- inverseVal
 getinverse <- function() inverse
 
 list(set = set, get = get, 
      getinverse = getinverse,
      setinverse = setinverse)
}


## This function first checks to see if the matrix inverse exists
## If it does not, it calculates the matrix inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  ## Get Matrix
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  
  inverse
}
