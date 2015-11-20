## The below two functions will cache the inverse 
## of a matrix.

## makeCahceMatrix function creats a special "matrix",
## a list containing a function to set and get the value of the matrix
## and set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        inverse <<- function(x) with(svd(x), v %*% diag(1/d^2) %*% t(v))
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## cacheSolve function calculates the inverse of the special "matrix" created with the above function.
## It checks if the inverse has already been calculated first. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## Not only square invertiable matrixes but all invertiable matrixes
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data,...)
        x$setinverse(m)
        m
}
