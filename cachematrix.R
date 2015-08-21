## These functions will create a list containing four functions (set, get, setmean, getmean) 
## and the second will check to see if the inverse has been calculated and cached and 
## then, if not, calculate and store this value

## Create list of the four functions

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) 
	  {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks if inverse cached, otherwise calculates it

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
