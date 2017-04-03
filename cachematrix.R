makeCacheMatrix <- function(original.matrix = matrix()) {
        if (!is.matrix(original.matrix)) stop("It's not a matrix, please insert a matrix")
        inverse <- NULL
        set <- function(y) {
                original.matrix <<- y
                inverse <<- NULL
        }
        get <- function() original.matrix
        set.inverse <- function(solve) inverse <<- solve
        get.inverse <- function() inverse
        list(
                set = set, 
                get = get,
                set.inverse = set.inverse,
                get.inverse = get.inverse)
}
cacheSolve <- function(cacheable.matrix, ...) {
        invers <- cacheable.matrix$get.inverse()
        if(!is.null(invers)) {
                message("Getting cached inverse matrix")
                return(invers)
        }
        invers <- solve(cacheable.matrix$get())
        cacheable.matrix$set.inverse(invers)
        invers
}
R <- matrix(runif(16), ncol=4) 
R
MCM<-makeCacheMatrix(R)
MCM$get.inverse()
cacheSolve(MCM)
