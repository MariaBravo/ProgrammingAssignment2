
## General description
## ===================
## The script implements 2 functions: 
## makeCacheMatrix -> This function creates a special "matrix" object that can cache its inverse.
## cacheSolve -> This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## It is assumed that the matrix supplied is always invertible.
## Example:
## 
## > source.with.encoding('cachematrix.R', encoding='UTF-8')
## > mtx <- matrix(c(1,2,-3, 11,-12,13, -21,22,23), nrow = 3, ncol = 3)
## > mtx
## [,1] [,2] [,3]
## [1,]    1   11  -21
## [2,]    2  -12   22
## [3,]   -3   13   23
## > mtxCached <- makeCacheMatrix(mtx)
## > mtxCachedInv <- cacheSolve(mtxCached)
## > mtxCachedInv 
## [,1]       [,2]        [,3]
## [1,] 0.354797980 0.33207071 0.006313131
## [2,] 0.070707071 0.02525253 0.040404040
## [3,] 0.006313131 0.02904040 0.021464646
## 
## Proof of calculation:
## > solve(mtx)
## [,1]       [,2]        [,3]
## [1,] 0.354797980 0.33207071 0.006313131
## [2,] 0.070707071 0.02525253 0.040404040
## [3,] 0.006313131 0.02904040 0.021464646
##
## Proof of use of cache:
## > mtxCachedInv2 <- cacheSolve(mtxCached)
## I am getting cached data
##
## > mtxCachedInv2
## [,1]       [,2]        [,3]
## [1,] 0.354797980 0.33207071 0.006313131
## [2,] 0.070707071 0.02525253 0.040404040
## [3,] 0.006313131 0.02904040 0.021464646



## makeCacheMatrix
## ############################################################################
## This function gets a squared matrix as parameter and returns a special "matrix" that
## besides the numeric column and row attributes, contains a list of functions or methods:
##   - set : set the value of the matrix
##   - get : get the value of the matrix
##   - setinverse : set the value of the inverse
##   - getinverse : get the value of the inverse
## ############################################################################

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
## ############################################################################
## This function calculates the mean of the special "matrix" created with the 
## makeCacheMatrix function. 
## First, it checks to see if the mean has already been calculated. 
## If it is true, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean 
## in the cache via the setmean function.
## #############################################################################

cacheSolve <- function(x, ...) 
{
    m <- x$getinverse()
    if(!is.null(m)) 
    {
        message("I am getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m    
}







