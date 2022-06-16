## Put comments here that give an overall description of what your
## functions do

# The function makeCacheMatrix and cacheSolve calulate the inverse
#   of a square matrix. If the inverse matrix was previously solved,
#   the result is read from cache instead of calculating it.
#
# makeCacheMatrix makes a local store of the matrix and the solve
#   of the matrix.  When setting the local store of the matrix, the solve
#   of the matrix is cleared.
#
# cacheSolve uses a function from makeCacheMatrix to see if the 
#   inverse has been solved.  It will use the cached value if it exists.
#   Otherwise, it will calculate the value by calling a function within 
#   makeCacheMatrix and storing it.
#   It then emits the solved matrix.

## Write a short comment describing this function

# makeCacheMatrix defines a function that:
# 1. Sets a matrix to an internal variable, 
#      and clears the inverse solve
# 2. Gets that matrix previously stored
# 3. Stores the inverse of a square matrix
# 4. Gets a matrix solve that was previously stored


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

# cashSolve calculates an inverse matrix
# If the matrix inverse was previously solved, 
#   the result is read from the cache
# The parameters takes an object created by the 
#   function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}

## Testing 
# > mymat = matrix(c(1,0,-1,2),nrow=2)
# > mymat
#       [,1] [,2]
# [1,]    1   -1
# [2,]    0    2
# > solve(mymat)
#      [,1] [,2]
# [1,]    1  0.5
# [2,]    0  0.5
# > solve(solve(mymat))
#      [,1] [,2]
# [1,]    1   -1
# [2,]    0    2
#
# Using the new functions
# > z = makeCacheMatrix(mymat)
# > cacheSolve(z)
#      [,1] [,2]
# [1,]    1  0.5
# [2,]    0  0.5
# > cacheSolve(z)
# getting cached data
#      [,1] [,2]
# [1,]    1  0.5
# [2,]    0  0.5
