## The following two functions are used to create a special object that stores a matrix 
## and cache's the inverse of the matrix.


## The first functiom below creates a special object and return a list containing the matrix and the inverse of the matrix.
makeCacheMatrix <- function(x = matrix())  {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }## set the value of the matrix
    get <- function() x ## get the value of the matrix
    setinverse <- function(solve) m <<- solve ## set the value of the inversed matrix
    getinverse <- function() m  ## get the value 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function first check whether the inverse of the matrix has been calculated. If not, 
## it will continue calculate it, otherwise the stored value would be returned.
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

## Running the code
a<-matrix(1:4,2,2)
cacheSolve(makeCacheMatrix(a))
b<-cacheSolve(makeCacheMatrix(a))
a%*%b
