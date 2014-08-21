##
## Function makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse
## Input	:	x		: matrix object
##				debug	: if TRUE, enter verbose mode and displays messages.
## Output	:	Special "matrix" object made with a List with 4 functions:
##				set(), get(), setinverse() and getinverse()
##
makeCacheMatrix <- function(x = matrix(), debug = FALSE) {
	## initialize inverse matrix
    inverse <- NULL
    ## set the main matrix
    set <- function(y) {
        if (debug == TRUE) message('makeCacheMatrix: set()')
        x <<- y
        inverse <<- NULL
    }
    ## get the main matrix
    get <- function() {
        if (debug == TRUE) message('makeCacheMatrix: get()')
        x
    }
    ## set the inverse matrix
    setinverse <- function(inv) {
        if (debug == TRUE) message('makeCacheMatrix: setinverse()')
        inverse <<- inv
    }
    ## get the inverse matrix
    getinverse <- function() {
        if (debug == TRUE) message('makeCacheMatrix: getinverse()')
        inverse
    }
    ## return a list with the previous 4 functions
    list(
    	set = set,
    	get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

##
## Function cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## Input	:	x		: special "matrix" object constructed with function makeCacheMatrix
##				...		: aditional parameters to function solve()
## Output	:	inverse matrix of 'x'
##
cacheSolve <- function(x, ...) {
    ## try to get the cached inverse matrix of 'x'
    inverse <- x$getinverse()
    ## check if it's already cached
    if(!is.null(inverse)) {
    	message("getting cached data")
    	## return the inverse matrix of 'x'
		return(inverse)
    }
    ## calculate the inverse matrix of 'x'
    message("calculating inverse matrix")
    ## get the data of matrix 'x'
    data <- x$get()
    ## number of rows of matrix 'x' (to make an identity matrix)
    rows <- nrow(data)
    ## calculate the inverse matrix
    inverse <- solve(data, diag(rows), ...)
    ## save the answer in the cache of 'x'
    x$setinverse(inverse)
    ## return the inverse matrix of 'x'
    inverse
}