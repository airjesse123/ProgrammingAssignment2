## These functions will take a matrix and return
## its inverse. If the matrix inverse has been
## computed before, the cached matrix will be returned

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # This assigns the inverse matrix to NULL
        set <- function(y) { # Set the value of the original matrix
                x <<- y  
                m <<- NULL
        }
        get <- function() x # Get the value of the original matrix
        setinv <- function(inv) m <<- inv # Set the value of the matrix inverse
        getinv <- function() m # Get the value of the matrix inverse
        list(set = set, get = get, # List all parts of these functions in a 
                                   # vector and return this as the result of 
                                   # the main function
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <- function(x, ...) { # cacheSolve is a function of x, a matrix
        m <- x$getinv()          # assign m to be the getinv() function 
        if(!is.null(m)) {        # if m is null, there is no cache stored
                message("getting cached data") #if there is a cache, pull it
                return(m)        # returned the cached result if there & exit
        }
        data <- x$get()          # set the data matrix to the value of the input
        m <- solve(data, ...)    # solve for the inverse of the matrix, set to m
        x$setinv(m)              # use m to set the inverse
        m                        # return m (the matrix inverse)
}