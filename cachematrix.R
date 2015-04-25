## makeCacheMatrix enables setting a matrix,getting a matrix, setting the inverse and getting the inverse
## caches the matrix and its inverse
## example illustration on how to use it
## x <- matrix(c(1:4),nrow=2,ncol=2) ## sets x as a 2X2 matrix with rows as 1,3 and columns 2,4
## y <- makeCacheMatrix(x)
## to set the matrix: y$set(x)
## to get the matrix: y$get()
## to set the inverse: y$setinverse(solve(x))
## to get the inverse  y$getinverse()
makeCacheMatrix <- function(x = matrix()) {
x_inv <- NULL
set <- function(y){
  x <<- y
  x_inv <<- NULL
}
  
get <- function() x  
setinverse <- function(solve) x_inv <<- solve
getinverse <- function() x_inv
list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## CacheSolve computes inverse if the inverse is already not computed for the same matrix. 
## It first gets the inverse from cache and if the matrix is the same and the inverse is not null, it is computed.
## illustration: 
## x <- matrix(c(1:4),nrow=2,ncol=2) ## sets x as a 2X2 matrix with rows as 1,3 and columns 2,4
## y <- makeCacheMatrix(x)
## z <- cacheSolve(y)
## output z to get the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        y <- x$get()
        if (!is.null(inv)&& x==y){
                message("getting cached data")
                return(inv)
            }

        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
