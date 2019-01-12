## makeCacheMatrix() creates an R object that stores a square matrix
## and its inverse, calculated in the complementary cacheSolve function using solve.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y             ## y named differently from x for readability; 
                            ## <<- forces search in parent environment
        i <<- NULL          ## Sets the inverse as NULL in the parent environment
    }
    get <- function() x                        ## "getter" to return x from parent environment, makeCacheMatrix, since x is not defined in get
    setinverse <- function(solve) i <<- solve  ## setinverse assigns the input argument to i in the parent environment
    getinverse <- function() i                 ## "getter" to return i from parent environment, makeCacheMatrix, since i is not defined in get
    ## makeCacheMatrix now returns a list containing four functions,
    ##        set, get, setinverse, and getinverse,
    ## with each element of the list named for its corresponding function to make them easy
    ## to invoke using the $ operator, e.g., x$setinverse().
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve requires an argument that is returned by makeCacheMatrix() in order to
## retrieve the matrix inverse from the cached value that is stored in the 
## makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {                  ## If there is an inverse in cache, it is returned 
        message("getting cached data") ## with a message to indicate that the cache was used
        return(i)
    }
    data <- x$get()         ## Since there was no inverse in cache, 
    i <- solve(data, ...)   ## it is computed,
    x$setinverse(i)         ## set in cache,
    i                       ## and returned.
}
