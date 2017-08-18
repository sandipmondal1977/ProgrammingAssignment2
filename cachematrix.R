##Following two functions are used to create a special object that stores a 
##numeric matrix and cache's its inverse matrix.


## The first function, makeCacheMatrix creates a special "matrix", which is a 
##list containing a function to

##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse matrix
##4. get the value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
##The function below calculates the inverse matrix of the special "matrix" 
##created with makeCacheMatrix function. However, it first checks to see if the inverse has 
##already been calculated cached. If so, it gets the it from the cache and skips the computation. 
##Otherwise, it re calculates inverse matrix of the data and sets the value of the in the cache via the setinverse function.



cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
