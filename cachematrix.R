## As matrix inversion is usually a costly computation it can be beneficial to 
## cache the inverse of a matrix rather than compute it repeatedly. 
## The following two functions cache the inverse of a matrix


## Function makeCacheMatrix makes a special matrix object that can cache its 
##inverse to reduce computing load.  



makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function cacheSolve either computes the inverse of the special matrix or if 
##the inverse is already calculated and the matrix has not changed it retrieves 
##the inverse from the cache. It assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}





