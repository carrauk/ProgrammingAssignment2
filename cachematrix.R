## Assignment submission for Week 3 assignment of R-Programming Course (Coursera).
## 2 functions to create a CacheMatrix 

## makeCacheMetrix - takes a matrix (and stores it) and can cache the result of the inverse calculated 
## with the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
        # varaible to store inverse (when calculated)
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_m <<- inverse
        getinverse <- function() inv_m
        
        # returns the following list - if variable printed
        list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## cacheSolve - takes a CacheMatrix - and returns its inverse using a cached result if available.
cacheSolve <- function(x, ...) {
        ## get the contents of the cache for the inverse calculation ...
        inv_m <- x$getinverse()
        # if found get the cached result ...
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        # otherwise calculate the inverse ...
        data <- x$get()
        inv_m <- solve(data, ...)
        # cache the result for future reference ...
        x$setinverse(inv_m)
        # return the inverse of the matrix;
        inv_m
}
