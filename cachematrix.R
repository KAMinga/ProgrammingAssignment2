## Caching the inverse of a matrix rather than compute it repeatedly
## through a pair of functions: 
## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), then
## the cachesolve retrieves the inverse from the cache.

## Creates an object that contains four functions: 
## set(), get(), setmean(), and getmean() and 
## two data objects: 
## x and m

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<-NULL
        }
    get <- function() x
    setsolveX <- function(solve) m <<- solve
    getsolveX <- function() m
    list(set = set, get = get, 
         setsolveX = setsolveX, 
         getsolveX = getsolveX)
}


## cacheSolve retrives the inverse of the matrix, and if
## it is not in cache, then it calculates it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolveX()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
    data <- x$get()
    m <- solve(data)
    x$setsolveX(m)
    m
}
