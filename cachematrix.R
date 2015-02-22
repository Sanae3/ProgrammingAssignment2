## makeCacheMatrix is a function, capable of making a matrix object that can cache its inverse matrix. First of all
## it sets the value of the matrix, then gets the value of it. Secondly, it sets the value of the inverse matrix, then gets the value.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## inv will serve as a repository for the value of the inverse, this line will make sure the value of inv is cleared
        set <- function(y) { ## this sets the value of the inverse in the cache
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## returns the value of x, which is the same as specified in the set function
        setinverse <- function(inverse) inv <<- inverse ##caches the inverse of a matrix for future reference
        getinverse <- function() inv ## reports the inverse matrix
        list(set = set, get = get, ## returns a list of all the steps incorporated in makeCacheMatrix
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve on its turn calculates the inverse of the matrix made with makeCacheMatrix.
## cacheSolve checks first whether or not the inverse was already calculated. If it was already done, then it gets the inverse matrix from the cache and returns the value.
## This saves a lot of time as it just needs to fetch the pre-calculated result from the cache. However, if the inverse is not available, then cacheSolve calculates the inverse of the provided
## matrix and sets the inverse value in the cache via the function setmean.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() ##retrieves the inverse from cache 
        if(!is.null(inv)) {  ##if the inverse value is not null, meaning that there is a pre-calculated inverse matrix in the cache
                message("getting cached data") ##then it returns that cached value
                return(inv)
        }
        data <- x$get() ##if this inverse is not found however, then it 
        inv <- solve(data, ...) ##calculates the inverse of the provided argument in the function
        x$setinverse(inv) ##caches the calculated inverse 
        inv ##returns the inverse matrix
}
