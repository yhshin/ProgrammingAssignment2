## 
## makeCacheMatrix() caches a given matrix and its inverse matrix, and
## wraps them with getters and setters to access it.
##   - cached values can be obtained by get() and getInvMatrix() functions.
##   - new values are set by set() and setInvMatrix() function.
##
## cacheSolve() operates on the cached matrix and returns cached inverse.
## If new matrix is assigned to the cache, it recalculates inverse and
## stores it to the cache.
##
## Example:
##
##   > m1 <- matrix(1:4, 2,2)
##   > mc <- makeCacheMatrix(m1)  # mc$x <- m, mc$invM <- NULL
##
##   > cacheSolve(mc)            # calculate inverse of mc$x
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##
##   > cacheSolve(mc)            # returns pre-calculated value
##   getting cached data
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##
##   > m2 <- matrix(5:8, 2,2)
##   > mc$set(m2)          # resets mc$x and mc$invM
##
##   > cacheSolve(mc)      # recalculate inverse of mc$x, for it is reset     [,1] [,2]
##        [,1] [,2]
##   [1,]   -4  3.5
##   [2,]    3 -2.5
##
##   > cacheSolve(mc)      # returns pre-calculated value
##   getting cached data
##        [,1] [,2]
##   [1,]   -4  3.5
##   [2,]    3 -2.5
##

##
## This function wraps a given mxtrix with functions 
## to get and set both the matrix and its inverse.
##
## NOTE that no actual calculattion is done in here. 
##
## Argument 
##   x  : n*n inversible matrix
##
## Return
##   A list of operations on the cached values
##     - get(), set()
##     - getInvMatrix(), setInvMatrix()
##
makeCacheMatrix <- function(x = matrix()) {
    
    # create the variable to cache inverse of x
    invM <- NULL        # to modify this from inside a nested function,
                        # spureassignment op (<<-) is required !
    
    # set a new matrix to be wrapped
    set <- function(y) {
        invM <<- NULL   # NOTE: if '<-' is used, it will create a local variable
        x <<- y         #       to this set() function with the same name as in  
    }                   #       parent function
    
    # return the current matrix
    get <- function() x
    
    # cache given matrix as an inverse of x
    setInvMatrix <- function(m) {
        invM <<- m
    }
    
    # returns cached inverse matrix
    getInvMatrix <- function() invM
    
    # return a list of operations to be used with this 
    list(set = set, get = get, 
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## Finds an inverse of a matrix cached by makeCacheMatrix() function.
## If the inverse of the matrix is cached, simply returns it.
## Or recalculate it when cached matrix has been changed.
##
## Arguments
##   x   : cached matrix returned by makeCacheMatrix().
##   ... : optional arguments to be passed to solve() function.
##
## Return
##   cached inverse matrix if not null, or
##   newly calculated inverse
##
cacheSolve <- function(x, ...) {
    ## Get cached inverse matrix, and return it if not null
    minv <- x$getInvMatrix()
    if (!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    ## cached inverse is null; x has a new matrix.
    ## find the inverse ot new matrix by using solve() function.
    m <- x$get()
    minv <- solve(m, ...)
    ## cache the solution to x 
    x$setInvMatrix(minv)
    ## and return it to the caller
    minv
}
