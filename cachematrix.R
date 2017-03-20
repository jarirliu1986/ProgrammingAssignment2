## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create the makeCashMatrix function, which can be used to get and modify the matrix and it's reverse
##CREATED BY ZYL
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## used to cache and set the reverse matrix of given matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInverse(inv)
        inv
}
