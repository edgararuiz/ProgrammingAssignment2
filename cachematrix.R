## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## - Verify that the variable passed is a matrix
        if(!is.matrix(x)){
                stop("variable not a matrix")
        }
        
        m <- NULL
        ## - Function that sets the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## - Function that passes the matrix 
        get <- function() x
        ## - Function that executes loads the inverse matrix
        setmatrix <- function(solve) m <<- solve
        ## - Function that retrieves the inverse matrix 
        getmatrix <- function() m
        list(
                set = set,
                get = get,
                setmatrix = setmatrix,
                getmatrix = getmatrix)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## - Getting the inverse matrix
        m <- x$getmatrix()
        ## - Checking if the inverse matrix has been cached 
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        ## - If the inverse matrix is not cached it retrieves the matrix, 
        ## - runs the solve function and loads the inverse results
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
        
        ## - Confirmed functionality with a 2x2 matrix of 1,2,3,4 
        ## - it returned -2,-1,1.5,-0.5
}
