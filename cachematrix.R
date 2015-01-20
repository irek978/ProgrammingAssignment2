## The following functions can be done to caching the inverse of a matrix.
## There are two funtions:

## 1) makeCacheMatrix where is created a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, 
         setinv=setinv, getinv=getinv)
}

## 2) cacheSolve where is computed the inverse of the special matrix returned by 
##    makeCacheMatrix above. If the inverse has alredy been calculated (and the matrix 
##    has not chanced) then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    r <- x$getinv()
    if(!is.null(r)) {
        message("getting cached data.")
        return(r)
    }
    data <- x$get()
    r <- solve(data)
    x$setinv(r)
    r
}

## test for verify the functions
## x<-matrix(c(1,2,1,3),nrow=2,ncol=2)
## xCach<-makeCacheMatrix(x)
## testInv<-cacheSolve(xCach)
## testInv
