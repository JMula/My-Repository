## These functions return the inverse of a given matrix.
## To optimize computational resources, the formula is
## also able to retrieve values previously calculated
## and stored in the cache

## This function creates a special "matrix" object that can save 
## to the cache along with its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        i <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                i <<- NULL
        }
        get <- function() x
        # Next 4 functions are used to save/call a matrix and its
        # inverse to/from the cache
        setcache <- function(gtc) m <<- gtc
        getcache <- function() m
        setinv <- function(inv) i <<- inv
        getinv <- function() i                
        list(set = set, get = get, setcache = setcache, getcache = getcache, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        m <- x$getcache()
        i <- x$getinv()
        # to fulfill requirements (inverse has already been 
        # calculated and matrix has not changed) the formula checks 
        # if matrix x was previously stored in the cache.If it was, 
        # and therefore we would get an already calculated inverse 
        # matrix from the cache, returns this matrix along with a message.
        if(!is.null(m)) {
                message("getting cached data")
                return(i)
        }
        # if the condition is not fulfilled, function calculates inverse,
        # before printing, saves original and inverse matrices to cache.
        a<- x$get()
        i <- solve(a, ...)
        x$setcache(a)
        x$setinv(i)
        i
}
