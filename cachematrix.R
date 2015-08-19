## Put comments here that give an overall description of what your
## functions do
##
##      Provide cache to return (calculate or retrieve) inverse of a given matrix
##      Use: 
##              given matrix:   m_init
##              cache:          c1 <- makeCacheMatrix(m_init)
##              return call:    cacheSolve(c1)
##              prove inverse:  cacheSolve%*%m_init
##

## Write a short comment describing this function
##
##      'makeCacheMatrix' creates cache in parent evn, and provides set/get methods
##      Use:
##              given matrix (m_init) passed in as arg
##
##      Internals:
##              creates cache
##              caches inverse in parent env
##              retrieves inverse from parent env
##              returns list of set/get methods
##
makeCacheMatrix <- function(x = matrix()) {
        mx <- NULL
        set <- function(y) {
                x <<- y
                mx <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mx <<- solve
        getinverse <- function() mx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##
##      'cacheSolve' uses methods of 'makeCacheMatrix' to manage cache of 
##              inverse of given matrix
##
##      Use:
##              cache (c1) passed in as arg
##              
##      Internals:
##              if cached inverse is not NULL, return it
##              else calculate inverse, store inverse in cache, and return inverse
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mx <- x$getinverse()
        if(!is.null(mx)) {
                message('getting cached data')
                return(mx)
        }
        data <- x$get()
        mx <- solve(data)
        x$setinverse(mx)
        mx
}

