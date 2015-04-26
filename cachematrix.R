## These functions return the inverse of an input matrix in an efficient
## manner by checking if the inverse has already been computed and only
## computing the inverse if the answer is not in memory.

## This function contains other functions (used in cacheSolve below) that can 
## substiute another input matrix y for input matrix x ('setnew'), retrieve the 
## input matrix ('get'), store as i a matrix called inverse that has been fixed 
## ('setinverse') and retrieve the stored matrix i ('getinverse').
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setnew <- function(y){
                   x <<- y
                   i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list (setnew = setnew, get=get,
              setinverse=setinverse, getinverse=getinverse)
}

## This function returns the inverse of an input matrix x. If the inverse
## is available in memory, it is returned from memory. If not, it is 
## computed, saved in memory and returned.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)){
                message ("getting cached data")
                return (i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}

