# Caching Inverse of a Matrix 

# Example usage
# a <- matrix(rnorm(4), nrow = 2, ncol = 2)
# a2 <- makeCacheMatrix(a)
# cacheSolve(a2)
#           [,1]      [,2]
# [1,] 0.9045365 -1.360898
# [2,] 0.7844247  1.007418
# cacheSolve(a2)
# getting cached data
#             [,1]      [,2]
# [1,]  0.5091137 0.6877499
# [2,] -0.3964207 0.4571209

# Creates a matrix that can cache it's inverse

# Returns:
#   A matrix with functions to get/set value & get/set inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL 
        
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function () {
                x
        }
        
        setinverse <- function (BOOM) m <<- BOOM
        
        getinverse <- function () m
        
## return list of functions for matrix
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

# Computes the inverse of a matrix. If the inverse has already been
# calculated before, the cached inverse is returned.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m <- x$get()
        m <- solve(m)
        x$setinverse(m)
        m
}
