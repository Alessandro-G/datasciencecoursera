## These functions together will calculate the inverse of a matrix and 
## cache the inverse matrix in memory.

## This function takes the input matrix, initializes the i object which
## stores the inverse matrix. It creates the setters and getters which 
## will be in the makeCacheMatrix vector.

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y){
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list( set=set, get=get,
              setinv=setinv,
              getinv=getinv)
}

## This function takes the makeCacheMatrix vector as an argument and 
## returns an inverse matrix. It either retrieves the cached value of 
## the inverse matrix, or calculates it anew and returns it. 

cacheSolve <- function(x, ...){
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached inverse matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

