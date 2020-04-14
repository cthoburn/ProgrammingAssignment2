## Chris Thoburn 04-14-20
## These functions are part of a programming exercise to learn how to
## store/use a cache of computationally expensive results (matrix inversion).

## This function creates a special "matrix" that can cache it's inverse.
## (a list of functions to set/get a matrix and set/get the inversion)
## we use <<- to assign in a different environment
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                      ## set matrix
                x <<- y                           ##  load new matrix
                m <<- NULL                        ##  destroy old cached m
        }
        get <- function() x                       ## return matrix
        setinverse <- function(solve) m <<- solve ## set results in m
        getinverse <- function() m                ## return cached m
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}  ## end makeCacheMatrix


## This function computes the inverse of a special "matrix" created by
## makeCacheMatrix (defined above).  If the inverse is already calculated
## for the matrix then it returns the copy from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                       ## check for previous cache results
        if(!is.null(m)) {
                message("getting cached results")
        }
        else {                                    ## no cache so calculate
                d <- x$get()                      ##  get the matrix
                m <- solve(d, ...)                ##  calculate inversion
                x$setinverse(m)                   ##  set inversion results in cache
        }
        return(m)                                 ## return result
}
