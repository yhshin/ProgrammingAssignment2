## 
## **makeCacheMatrix()** caches a given matrix and its inverse matrix, and
## wraps them with getters and setters to access it.
##   - cached values can be obtained by get() and getInvMatrix() functions.
##   - new values are set by set() and setInvMatrix() function.
##
## **NOTE** that the above function normally can **_not_** be called from 
##      outside of the function. That is why a list whose elements are
##      bound to the function is returned from the function !
##
## **cacheSolve()** operates on the cached matrix and returns cached inverse.
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
## NOTE: There is a post about possible bug with ... arg.
##         https://class.coursera.org/rprog-011/forum/thread?thread_id=356
##       But in case of solving inverse matrix, we can ignore changing ... args
##       because it does NOT affect the result.
##
##       Just as a note for future use, an example of getInvMatrix(...) function
##       is implemented and the original version is commented out.
##
## Reference: 
##   1. How to handle ... argument in user function
##        http://stackoverflow.com/questions/3057341/how-to-use-rs-ellipsis-feature-when-writing-your-own-function
##

##
## makeCacheMatri() function creates and returns a list object which has 
## elements bounded to get and set functions.  It is this boundings enabling
## us to call those functions outside of makeCacheMatrix() function!
##
## NOTE:
##   Actual calculattion is being performed in cacheSolve() function.
##
## Arguments:
##   x  : n*n inversible matrix
##
## Return:
##   A list whose elements are get and set functions to the cached values
##     - get(), set()
##     - getInvMatrix(), setInvMatrix()
##
makeCacheMatrix <- function(x = matrix()) {
    
    # create the variable to cache inverse of x
    invM <- NULL        # to modify this from inside a nested function,
                        # spureassignment op (<<-) is required !
    
    # Additional member to handle a case posted in the thread
    #   https://class.coursera.org/rprog-011/forum/thread?thread_id=356
    #
    dots.cached <- list()
    
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
    ##getInvMatrix <- function() { invM }
    #
    # We may pass different option to solveCache() function, which in prnciple
    # should be caught and be handled properly. But in martix invsers case,
    # other options does *not* affect the result so it can be ignored.
    #
    # This is a very simpl example of handling ... arg for future reference.
    getInvMatrix <- function(...) {
        # Check ... arg.
        #   + if ... is empty, simply re-use current 'invM'.
        #   + if it is not empty and is different from cached list, 
        #        reset 'invM' to NULL, and
        #        save new ... arg list to dots.cached.
        dots.new <- list(...)
        if (length(dots.new) && !identical(dots.cached, dots.new)) {
            message("dots(...) arg has been changed")
            dots.cached <<- dots.new
            invM <<- NULL
        }
        invM
    }
    
    # return a list of operations to be used with this
    # *NOTEs* 
    #   1. The above functions normally can **_not_** be called from outside
    #      of the function. That is why the following list whose elements are
    #      bound to the function is returned !
    #   2. List elements can have any names such as 'hello', 'world', etc.
    #      but the same names are chosen to make easy to know what they are for.
    #
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
    minv <- x$getInvMatrix(...)
    
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
