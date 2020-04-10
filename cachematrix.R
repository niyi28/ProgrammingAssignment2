## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix with input of a matrix, creates a list including
# a function that sets the matrix, returns the set matrix,  
# a function for the inversion of set matrix and a function to get the 
#inversion function

makeCacheMatrix <- function(x = matrix()) {
        MatrixM <- NULL
        set <- function(y) {
                x <<- y
                MatrixM <<- NULL
        }
        get <- function() x
        
        setSolve <- function(solve) MatrixM <<- solve
        
        getSolve <- function() MatrixM
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## Write a short comment describing this function
# The cacheSolve function  takes the returned list from makeCacheMatrix and
# inverses the matrix already inputed in the makeCacheMatrix function.
# if the matrix is non-square, NULL is returned and 
# if not the inverse of the matrix is returned
# ... allows additional arguments to be provided 

cacheSolve <- function(x, ...) {
        MatrixM <- x$getSolve()
        if(!is.null(MatrixM)) {
                message("getting cached inverse data")
                return(MatrixM)
        }
        data <- x$get()   
        if (isTRUE(sqrt(nrow(data) * ncol(data))==nrow(data))){
                MatrixM <- solve(data, ...)
        }
        
        x$setSolve(MatrixM)
        MatrixM
} 

