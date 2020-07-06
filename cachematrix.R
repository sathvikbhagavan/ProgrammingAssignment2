## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Comment: 

## This function helps in making cache/separate objects using the concept of lexical scoping 
## such that if a matrix existing its parent environment is used 
## for calculating its inverse again and again, instead of calculating it again and again, we can look up if its 
## inverse which would have been calculated once and stored and hence we can use its value anytime we want

## It consists:
## 1. set() - set the value of the matrix object
## 2. get() - get the value of the matrix object
## 3. getInverse() - get the value of inverse of the matrix object if cached
## 4. setInverse() - set the value of the inverse of the matrix object if not cached for future references
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


## Usage:

# Make cached object (which is a list of functions) of a matrix object
cache_m1 <- makeCacheMatrix(matrix(c(1,4,3,2), nrow = 2, ncol = 2))
# As inverse is not cached the first time it will calculated and set
cacheSolve(cache_m1)
# We can directly use the cache
cacheSolve(cache_m1)
# We can change the cached matrix object:
cache_m1$set(matrix(c(3,2,1,4), nrow = 2, ncol = 2))
# We can then again use it for inverse matrix calculations