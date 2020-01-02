##Comparing to compute matrix inversion repeatedly, cashing it is a better
## choice. This functioncan create a special object that stores a matrix and 
## caches its inverse.

## This special matrix can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
      x <<- y
      inv <<- NULL
}
get <- function()x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set
     get = get
     setInverse = setInverse
     getInverse = getInverse
}


## This function compute the inverse of the matrix above.
## If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
   inv <- x$getInverse()
   if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
}
