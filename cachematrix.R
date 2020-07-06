## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Comment: 

## This function helps in making cache/separate objects using the concept of lexical scoping 
## such that if a matrix existing its parent environment is used 
## for calculating its inverse again and again, instead of calculating it again and again, we can look up if its 
## inverse which would have been calculated once and stored and hence we can use its value anytime we want
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() {return(x)}
    setInverse <- function(i) {
      inverse <<- i
    }
    getInverse <- function() {return(inverse)}
    return(list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse))
}


## Write a short comment describing this function
## Comment: 

## This is the driver function for calculating the inverse. First, we cache the matrix and then we call 
## this function passing the return value of makeCacheMatrix for calculating its inverse
## If the inverse of a particular matrix object has been 
## calculated before, it can be directly retrieved without need for calculating again which can be useful for saving computational
## time.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    return(inverse)
}
