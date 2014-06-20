## The first function, `makeCacheMatrix` creates a list with 4 functions:
##[1] set the value of a matrix
##[2] return the value of the matrix
##[3] set the value of the inverse of the matrix, applying the solve function
##[4] return the value of the inverse of the matrix

## This function will return a list! assign it to a variable (eg. x)
## create a matriz and assing it to another variable (eg. y)
## to create a a matrix, call x$set(y)
## to see the created matrix, call x$get()

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## this function will calcuate the inverse of the matrix created in the first function
## call it using cacheSolve(x)

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
        ## Return a matrix that is the inverse of 'x'
}
