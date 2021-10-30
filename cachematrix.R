## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y){
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) Inv <<- Inverse
        getInverse <- function() Inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix,...)
        x$setInverse(inv)
        inv
}
test1 <- matrix(rnorm(16, 2, 0.5), 4, 4)
test2 <- makeCacheMatrix(test1)
cacheSolve(test2)
