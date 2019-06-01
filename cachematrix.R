## This functions are encapsulation of matrix for caching in 
## memory some heavy processing functions

## This extends matrix implementation to add cache layer when 
## calculating the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## calculates the inverse of a cacheMatrix 
## (and uses the pre-calculated cache if possible)
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
    ## Return a matrix that is the inverse of 'x'
}

## test code -- simple det!=0 matrix
# A <- makeCacheMatrix(
#     matrix( c(5, 1, 0,
#               3,-1, 2,
#               4, 0,-1), nrow=3, byrow=TRUE))
# cacheSolve(A)
# cacheSolve(A) # this time uses cache

## test code -- creates a square matrix much bigger to teest performance gain
# matrixSize <- 1000
# t <- makeCacheMatrix(
#     matrix( rnorm(matrixSize*matrixSize, mean=1), nrow=matrixSize, byrow=TRUE))
# head(cacheSolve(t),1)
# head(cacheSolve(t),1)  # this time uses cache


