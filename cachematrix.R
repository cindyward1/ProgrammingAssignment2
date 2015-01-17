## Together, these two functions calculate, cache and return the inverse of a matrix

## makeCacheMatrix returns a list containing functions to:
## 1) Set the value of the input matrix (set)
## 2) Get the value of the input matrix (get)
## 3) Set the value of the inverse of the input matrix (setinverse)
## 4) Get the value of the inverse of the input matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
    inverted_matrix <- NULL
    set <- function(placeholder) {
        x <<- placeholder
        inverted_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverted_matrix <<- solve
    getinverse <- function() inverted_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve calculates the inverse of the input matrix by using the
## special "vector" returned from makeCacheMatrix. It checks the cache 
## to find the inverse. If the inverse is cached, it is returned, not computed.
## If the inverse is not cached, it is calculated, cached and returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted_matrix <- x$getinverse()
    if(!is.null(inverted_matrix)) {     # if the inverse is already cached
        return(inverted_matrix)         # returns inverse
    }
    data <- x$get()
    inverted_matrix <- solve(data, ...) # calculates inverse
    x$setinverse(inverted_matrix)       # caches inverse
    inverted_matrix                     # returns inverse
}
