## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Cache for the inverse
  inv <- NULL 
  
  # Sets the matrix and clears the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Gets the current matrix
  get <- function() x
  
  # Sets the inverse of the matrix in the cache
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Gets the cached inverse of the matrix
  getinverse <- function() inv
  
  # Returns a list of the above functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve function computes the inverse of the "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated and cached, it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Check if the inverse has already been calculated and if it is just return the cached inverse
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # If the inverse has not already been calculated, then get the matrix, compute its inverse, cache it, and return it
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}