# The functions solve and cache the inverse of a given matrix

# The function creates a matrix whose inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
  
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv) mInverse <<- inv
  getInverse <- function() mInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


# This function checks if the matrix returned by makeCacheMatrix()
# already has an inverse cached. If so, the function returns it. 
# If not, the function calculates the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  
  # setting inverse to matrix inverse from makeCacheMatrix()
  mInverse <- x$getInverse() 
  
  # if the inverse is already cached, return it
  if(!is.null(mInverse)) {
    message("The inverse has already been calculated")
    return(mInverse)
  }
  
  # if the inverse isn't already cached, solve it and return it
  matrix <- x$get()
  mInverse <- solve(matrix, ...)
  x$setInverse(mInverse)
  mInverse
  
}

