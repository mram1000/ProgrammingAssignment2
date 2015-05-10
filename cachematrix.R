## makeCacheMatrix - makes a special matrix with embedded functions to cache
## its inverse, and retrieve it if the matrix has not changed.


## creates a matrix along with functions to set, get the matrix; as well as its inverse

makeCacheMatrix <- function(x = matrix()) {

        ##set the inverse matrix to null upon creation
        inv <- NULL
        
        set <- function(y) {
                ## store the matrix in parent environment
                x <<- y
                
                ## reset the inverse to NULL if the matrix content has been changed(reset)
                inv <<- NULL
        }
        
        ## simply return the matrix
        get <- function() x
        
        ## used my cache Solve to set the inverse matrix (cache) after solving
        setInverse <- function(inverseMatrix) inv <<- inverseMatrix
        
        ## returns the cached copy of the inverse 
        getInverse <- function() inv
        
        ## list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## gets the inverse of the matrix from cache if exitst, otherwise solves for it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ##check if cached copy exists already, if so retrieve it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ##get the matrix data using the get function
        ##from original matrix
        data <- x$get()
        
        ##solve for the inverse
        inv <- solve(data)
        
        ##save the cache copy to original matrix object
        x$setInverse(inv)
        
        ##display the inverse
        inv
}

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
