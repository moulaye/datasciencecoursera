## These two functions interact to give the cached version 
## of the inverse of a matrix. cacheSolve uses makeCacheMatrix

## makeCacheMatrix: this function sets and gets the value of a matrix
## the function also sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##cacheSolve: this function returns the cached version of the inverse of a matrix
## or returns a newly calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 s <- x$setinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
