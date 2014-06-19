
# Example usage:
# > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the invMerse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached invMerse

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the invMerse
# 4. Get the value of the invMerse


makeCacheMatrix <- function(x = matrix()) {
    # invM will store the cached inverse matrix
    invM <- NULL


    # Setter for the matrix
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    # Getter for the matrix
    get <- function() {
	  x
    }


    # Setter for the inverse
    setinvM <- function(inverse) { 
	  invM <<- inverse
    }
    # Getter for the inverse
    getinvM <- function() {
	  invM
    }


    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinvM = setinvM, getinvM = getinvM)
}




# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
    invM <- x$getinvM()


    # If the inverse is already calculated, return it
    if (!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }


    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    invM <- solve(data, ...)


    # Cache the inverse
    x$setinvM(invM)


    # Return it
    invM
}
