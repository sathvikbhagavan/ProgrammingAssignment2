## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function helps in making cache/separate object such that if a matrix in its parent environment is used 
# for calculating its inverse again and again, instead of calculating is again and again, we can look up if its 
# calculated once and using its value anytime we want
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() {x}
    setInverse <- function(i) {
      inverse <<- i
    }
    getInverse <- function() {inverse}
    return(list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse))
}


## Write a short comment describing this function
# This is the driver function for calculating the inverse. If the inverse of a particular matrix object has been 
# calculated, it can be directly retrieved without need for calculating again which can be useful for saving computational
# time.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
      print('Cache data!')
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    return(inverse)
    ## Return a matrix that is the inverse of 'x'
}
