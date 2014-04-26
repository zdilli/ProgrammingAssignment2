## The functions in this file allow the user to store a special 
## "matrix" which can be cached along with its inverse in a 
## list structure.

## makeCacheMatrix returns a list with functions to set and 
## retrieve a matrix and its inverse.  Whereas the vector version
## of these programs had used NULL to initialize the mean of the 
## vector, here we use an empty matrix to initialize the inverse 
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- matrix(, nrow = 0, ncol = 0)
    set <- function(y){
        x <<- y
        xinv <<- matrix(, nrow = 0, ncol = 0)
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Checks if the inverse of a matrix has been cached and returns it if
## it has been.  If not, it calculates and caches the inverse as 
## well as returning it, so that when this is ran for a second time
## on the same matrix, it will not recalculate but indicate
## it is getting cached data.
## Note that the empty matrix used to initialize the matrix inverse
## in makeCacheMatrix(x) will return FALSE for is.numeric().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    xinv <- x$getinv()
    if(is.numeric(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}