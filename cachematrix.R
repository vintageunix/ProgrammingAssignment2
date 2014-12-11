## Function : cacheMatrix
## Purpose  : Create an object which takes in a matrix and    
##            creates the basic accessor functions for the matrix 
##            and its inverse

makeCacheMatrix <- function(x = matrix()) {
 ## Inv of the matrix reset everytime the cache is created
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Return the original matrix
    get <- function() x
    ## Set the Inverse
    setinv <- function(solve) inv <<- solve
    ## return the cached value
    getinv <- function() inv
    ## Method defnitions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function : cacheSolve
## Purpose  : Calculate the inverse of a matix and cache the value  
##            so that future calls can retrieve from cache if there
##            are no changes

cacheSolve <- function(x, ...) {
    ## Get the Inverse of X
    m <- x$getinv()
    ## Not Null implies that there is a cached value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Inverse not cached Get the data set
    data <- x$get()
    ## solve is the function which calcs the inverse
    m <- solve(data, ...)
    ## Store it
    x$setinv(m)
    m
}
