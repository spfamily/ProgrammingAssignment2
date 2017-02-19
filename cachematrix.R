
# Defines 4 functions
# Function set:
# 1. Assigns the value on the right side of the operator (y) to an object 
#    in the parent environment named by the object on the left side of the operator (x)
# 2. Clears any value of inv that had been cached by a prior execution of makeCacheMatrix().
#    If there is already a valid inverse of matrix cached in inv, whenever x is reset, the value of inv cached 
#    in the memory is cleared, forcing subsequent calls to makeCacheMatrix() to recalculate.
#
# Function get:
# 1. Retrieves matrix x from the parent environment of makeCacheMatrix() because x is not defined within get().
#
# Function setinv:
# 1. Set inversed matrix in cache. Since inv is defined in the parent environment and we need to access it after 
#    setinv() completes, the code uses the <<- form of the assignment operator to assign the input argument to 
#    the value of inv in the parent environment.
#
# Function getinv:
# 1. Retrieves inverse of matrix 
#
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize object to be used
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse 
        
        getinv <- function() inv
        
        ## Assigns each of the functions above as an element within a list(), 
        ## and returns it to the parent environment.
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Return the inverse of matrix x if it has already been cached
# Or calculate & return the inverse of matrix if cache is empty
cacheSolve <- function(x, ...) {
        # Retrieves inverse of matrix
        inv <- x$getinv()
        
        # If the inversed matrix has already been calculated
        if (!is.null(inv)){
                # Retrieve it from the cache and return the inverse of the original matrix input. 
                message("Getting cached data!!!")
                return(inv)
        }
        
        # Otherwise, calculates the inverse 
        matrix_data = x$get()
        inv = solve(matrix_data, ...)
        
        # Sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}

