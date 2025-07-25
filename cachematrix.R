## Put comments here that give an overall description of what your
## functions do

# These functions work together to cache the inverse of a matrix.
# This helps avoid redundant computations by storing the inverse
## once it's been calculated.

## Write a short comment describing this function

# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse. It returns a list of functions to
# set/get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  set <- function(y) {
    x <<- y      # Update the matrix
    inv <<- NULL # Clear cached inverse
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  # Return a list of functions
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Write a short comment describing this function

# cacheSolve: This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix. If the inverse has already
# been calculated and cached, it retrieves it. Otherwise, it
# computes the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    # If the inverse is already cached, return it
    if (!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    
    # If not, compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)  # Compute inverse
    
    x$setinverse(inv)       # Cache it
    
    inv
}
  

