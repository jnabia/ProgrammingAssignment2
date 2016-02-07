
## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse
        
        setInverse <- function(inverse) inv <<- inverse
        ## get the value of the inverse
        
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
## returns the inverse of x and stores it in "incv"
        inv <- x$getInverse()
## sets the if-then condition
        if(!is.null(inv)) {
## checks if the inverse has been calculated 
## indicates there is cache and get inverse from the cache
                message("getting cached data")
                return(inv)
        }
## get the matrix from makeCacheMatrix
        data <- x$get()
## calculates inverse
        inv <- solve(data, ...)
## store the inverse in cache
        x$setInverse(inv)
        inv
}

##sample data
samp_matrix<-makeCacheMatrix(matrix(c(3,2,5,6),2,2))
samp_matrix$get()
samp_matrix$getInverse()
cacheSolve(samp_matrix)
cacheSolve(samp_matrix)
