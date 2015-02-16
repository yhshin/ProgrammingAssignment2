##
## This is a sctipr to test makeCacheMatrix() and cacheSolve() functions.
##
source("cachematrix.R")

testCacheMatrix <- function(n=100) {

    # Generate nxn random numbers to be used as matrix elements
    nelem <- n*n
    #elems <- sample(1:nelem, replace=TRUE)
    elems <- runif(nelem)
    
    # Create a test matrix
    m1 <- matrix(elems, n,n)
    
    # Create a wrapper object
    mc <- makeCacheMatrix(m1)
    
    # Solve the inverse matix
    message("--- calculating inverse... ---")
    system.time(cacheSolve(mc))
    
    # Print them
    message("printing part of matrix...")
    printMatrix(mc$get())

    message("printing part inverse matrix...")
    printMatrix(mc$getInvMatrix())
    
    # Let's calculate it again
    message("--- calculating inverse again... ---")
    system.time(cacheSolve(mc))
    
    # Now test ... arg
    message("--- Testing cacheSolve with ... argument. ---")
    cacheSolve(mc, tol=1e-15)
    
    message("--- Running cacheSolve again with no additional arg. ---")
    message("    It should simply return cached value.")
    cacheSolve(mc)
    
    #
    # Do it again for new matrix
    #
    message("\n\n=== Solve for new matrix ===")
    m2 <- matrix(runif(nelem), n,n)
    
    # Instead of creating a new cache, set new value to the cache
    mc$set(m2)
    
    # Solve the inverse matix
    message("--- calculating inverse of new cache value -")
    system.time(cacheSolve(mc))
    
    # Print them
    message("printing part of matrix")
    printMatrix(mc$get())
    
    message("printing part of inverse matrix")
    printMatrix(mc$getInvMatrix())
    
    # Let's calculate it again
    message("--- calculating inverse again ---")
    system.time(cacheSolve(mc))
}

#
# Print part of matrix
#
printMatrix <- function(m=matrix(), nhead=5) {
    if (is.null(m)) {
        warning("Null matrix is given")
        return(NULL)
    }
    nrow <- nrow(m)
    nshow <- min(nhead, nrow)
    mpart <- head(m[,1:nshow], nshow)
    print(mpart)
    return (NULL)
}