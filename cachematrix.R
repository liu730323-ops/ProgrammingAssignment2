## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Cache used to store the inverse matrix
  
  # Set up a new matrix and clear the cache.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the current matrix
  get <- function() x
  
  # Set the inverse matrix (store in cache)
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse matrix (from cache)
  getinverse <- function() inv
  
  # Returns a list containing these four inner functions.
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # If the inverse matrix is already in the cache, return directly.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, retrieve the matrix from the object, calculate its inverse, and write it back to the cache.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
