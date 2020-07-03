## The two functions below are create a matrix and cache its
## inverse.  This helps in not having to compute the inverse repeatedly

## This first function "makeCacheMatrix" below creates a matrix 
## object that is ableto cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This second function "cacheSolve" below calculates the 
## inverse of the matrix object created above.  It should 
## retrieve the inverse from the cache if the inverse has 
## already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        Matrx <- x$get()
        m <- solve(Matrx, ...)
        x$setInverse(m)
        m
}
