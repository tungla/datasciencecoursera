# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  inv <- NULL
  # set the value of the matrix
  set <- function( y ) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of inverse of the matrix
  setsolve <- function( solve ) inv <<- solve
  # get the value of inverse of the matrix
  getsolve <- function() inv
  list( set = set, get = get, setsolve = setsolve, getsolve = getsolve )
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix().
# If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function( x, ... ) {
  # get the value of inverse of the matrix
  inv <- x$getsolve()
  # the value of inverse found in a cashe
  if( !is.null( inv ) ) {
    message( "getting cached data" )
    return( inv )
  }
  # compute a new value of inverse of the matrix
  data <- x$get()
  inv <- solve( data, ... )
  # set the value of inverse in a cache
  x$setsolve( inv )
  # return the value of inverse
  inv
}