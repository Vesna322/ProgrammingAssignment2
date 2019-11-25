## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are two pairs of functions:
## 1. makeCacheMatrix - that creates a special "matrix" object that can cache its inverse,
## 2.cacheSolve - that computes the inverse of the special "matrix" retuned by makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

## 1. makeCacheMatrix: 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## 2.cacheSolve 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
## Testing of makeCacheMatrix and cacheSolve functions:

> source("ProgrammingAssignment2/cachematrix.R")
> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$set(matrix(c(4, 3, 1, 4), 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    4    1
[2,]    3    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
           [,1]        [,2]
[1,]  0.3076923 -0.07692308
[2,] -0.2307692  0.30769231
> my_matrix$getInverse()
           [,1]        [,2]
[1,]  0.3076923 -0.07692308
[2,] -0.2307692  0.30769231
