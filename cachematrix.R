## Solving for the inverse of a matrix can be computationally 
## intensive, so these functions cache (save) the results of
## already solved matrices in case you need to solve them again.

## To use these functions, first create a CacheMatrix from your
## original matrix 'm':
##
## > m2 <- makeCacheMatrix(m)
##
## Then, solve the matrix using the cacheSolve function:
##
## > cacheSolve(m2)  

## makeCacheMatrix creates a list of 4 functions
## to solve the matrix and save the results

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks to see if the matrix 'x' has already
## been solved. if so, it returns the previous result.
## if not, it solves the matrix and saves the result for later.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
