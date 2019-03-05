##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly

##following are a pair of functions that cache the inverse of a matrix.

##create a matrix that can cache its inverse
makeCacheMatrix <- function ( x= matrix())
{
        inv <- NULL
        set <- function(y)
        {
                x <<-y
                inv <<- NULL
        }
        get <- function() 
                x
        setinverse <- function(solve)
                inv<<-solve
        getinverse <- function()
                inv
        list(set = set, get = get, setinverse = setinverse,  getinverse = getinverse)
}

##return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...)
{
        inv <- x$getinverse()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}