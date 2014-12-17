## Calculating the inversion of a matrix is a costly computation.
## It is better to do it once, and store the result for future reference. 

# makeCacheMatrix creates a matrix, inverts the matrix, and stores the inversion of that matrix.

makeCacheMatrix <- function(x = matrix()) { ## The imput will be a matrix. 
        m <- NULL ## m is the inversion of the matrix, and is set to NULL when a matrix is created.
        set <- function(y) { 
                x <<- y
                m <<- NULL # method for cacheSolve() to get the values for x or for m and for setting the inversion.
        }
        get <- function() x # returs the value of the original matrix
        setsolve <- function(solve) m <<- solve #sets the inversion
        getsolve <- function() m # is used to get the inversion when it has been calculated before
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve) # methods for makeCacheMatrix
}

## cacheSolve checks whether the matrix has already been inverted. 
## If that is the case it returns it. Else it calculates, stores and returns it.

cacheSolve <- function(x, ...) { # uses the object created by makeCacheMatrix
        m <- x$getsolve() # gets the value of the solved matrix
        if(!is.null(m)) { # if the matrix was inverted (not NUll)
                message("getting cached data") # this message is displayed in the console
                return(m) # and returns the inverted matrix
        }
        data <- x$get() # if x$getsolve() is NULL
        m <- solve(data, ...) # invert the matrix
        x$setsolve(m) # store the inverted matrix 
        m # return the inverted matrix
}