## This function will cache the inverse of matrices and return cached values if available. 
## There are 2 steps to using the function, first put the matrix you want in the first equation (makeCacheMatrix) 
## Next solve the inverse of the matrix using the second equation (cacheSolve) using the output of makeCacheMatrix. 
## For this function to work the input needs to be invertable

## This function will store the cache and sets the functions for cacheSolve
## I closely used the example from the assignment, but decided not to include the "set" function as wasn't doing anything. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function with check and return a cached inverse matrix, if there is no cache it will generate a new inverse matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
