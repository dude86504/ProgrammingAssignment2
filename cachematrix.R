## Matrix Inverse
## The first function, Makecachematrix, stores a matrix, but also caches the
## inverse matrix after cachesolve() as inverted it. 

## The function below creates a list object that makes it possible for 
## cacheSolve() produce an inverted matrix, but also store it in 
## an makecachematrix() so that the calculation only has to be done once 
## as long as the makecachematrix() object has not been overwritten by a 
## new input matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## The input for the x argument is the makecachematrix object.
## it first tries to collect the inverse matrix from the x object.
## if it doesn't exist it will then create the inverse. If it does exist
## it will simply return the matrix allready calculated and stored in the
## make cachematrix object. 

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
