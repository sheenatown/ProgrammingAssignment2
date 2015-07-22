## Together makeCacheMatrix and cacheSolve store 
## and calculate the inverse of a matrix

## When passed an invertible matrix, makeCacheMatrix
## contains a list of functions to set and get the 
## matrix and to set and get the inverse of the 
## original matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  #sets value of x when passed y
                x <<- y
                m <<- NULL
        }
        get <- function() x #gets the value of x
        setinv <- function(inv) m <<- inv #sets the value of inv
        getinv <- function() m #gets the value inv, as m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## When passed a variable containing the results of 
## makeCacheMatrix that has been passed and invertible 
## matrix, cacheSolve checks to see if there is a value
## stored in m (i.e. the inverse) and if there is it 
## returns this value along with a message. If m is 
## empty, it calculates the inverse of the Matrix using 
## the solve function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m  ## Return a matrix that is the inverse of 'x'
}
