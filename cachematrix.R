## Create a list that contains functions to retrieve, add a matrix, as well as add its inverse or just retrieve it ...
makeCacheMatrix <- function(x = matrix()) {
        if (!is.matrix(x)) stop("It's not a matrix, please insert a matrix")
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set.inverse <- function(solve) inverse <<- solve
        get.inverse <- function() inverse
        list(
                set = set, 
                get = get,
                set.inverse = set.inverse,
                get.inverse = get.inverse)
}
## Displays the inverse of the matrix or creates (with the "solve" function) this inverse and displays it
cacheSolve <- function(c, ...) {
        invers <- c$get.inverse()
        if(!is.null(invers)) {
                message("Getting cached inverse matrix")
                return(invers)
        }
        invers <- solve(c$get())
        c$set.inverse(invers)
        invers
}
R <- matrix(runif(16), ncol=4) 
R
MCM<-makeCacheMatrix(R)
MCM$get.inverse()
cacheSolve(MCM)
