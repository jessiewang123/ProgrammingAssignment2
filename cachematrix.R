## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        calinverse <- function(x) with(svd(X), v %*% diag(1/d^2) %*% t(v))
        setinverse <- function(calinverse) m <<- inverse
        getinverse <- function() m
        lsit(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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
