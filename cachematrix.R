## provide caching capability for matrices and their inverse
#makeCacheMatrix is an R object containing a matrix and functions to read, write, store the inverse [solve] and get the inverse


makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) i <<- solve
                getsolve <- function() i
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}


## Allows the user to retrieve the inverse of a matrix, if it has already been calculated without need to recalculate
## This is for valid matrices only and doe not have validation logic
## If the inverse has not yet been calculated, this function will also set the cache for future speedier execution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
