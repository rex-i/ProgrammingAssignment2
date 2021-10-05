## The function will be used to create a matrix and customized objects within
## The makecachematrix is a function in which would create a matrix for it to be identified
makeCacheMatrix <- function(x = matrix()) {
        f <- NULL
        set <-function(y) {
                x <<- y
                f <<= NULL
}
get <- function() x
setinverse <- function(inverse) f <<- inverse
getinverse <- function() f
list(set = set,
     get = get,
     setinverse = setinverse,
     getinverse = getinverse)
      
}


## This function will solve the matrx's inversed values
## the inverse's value will be calculated by the chache using cacheSolve function

cacheSolve <- function(x, ...) {
        f <- x$getinverse()
if (!is.null(f)) {
        message("getting cached data")
                return(f)
}
        data <- x$get()
        f <- solve(data, ...)
        x$setinverse(f)
        f
}
         
