##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly

##following are a pair of functions that cache the inverse of a matrix.

##create a matrix that can cache its inverse
makeCacheMatrix <- function ( x= matrix())
{
        inv <- NULL
        ##set matrix values
        set <- function(y)
        {
                x <<-y
                inv <<- NULL
        }
        ##get matrix values
        get <- function() 
                x
        ##set inversed matrix values
        setinverse <- function(solve)
                inv<<-solve
        ##get inversed matrix values
        getinverse <- function()
                inv
        list(set = set, get = get, setinverse = setinverse,  getinverse = getinverse)
}

##return a matrix that is the inverse of 'x'

##If the inverse has already been calculated, 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...)
{
        ##get the inversed matrix value
        inv <- x$getinverse()
        ##if the inverse has already been calculated
        if(!is.null(inv))
        {
                
                message("getting cached data")
                return(inv)
        }
        ##if the inverse has not been calculated
        ##compute the inverse value
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
