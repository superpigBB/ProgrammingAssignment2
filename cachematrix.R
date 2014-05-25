## Put comments here that give an overall description of what your
## functions do
## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of a square matrix 
## 4.get the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                
        set <- function(y) {       #set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x        #get the value of the matrix
        setinverse <- function(inverse) m <<- inverse  #set the inverse of the matrix
        getinverse <- function() m     # get the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), the
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()    #get the inverse of matrix
        if(!is.null(m)) {      #if the inverse exists, retrieve the result from the cache
                message("getting cached data")
                return(m)
        }
        data <- x$get()       #if the inverse does not exists, recalculate the inverse of the matrix
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
