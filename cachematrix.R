## This file contains functions to calcualte inverse of a matrix and cache it.
## If matrix is not changed, repeated calls to calculate inverse would result in
## calculation of inverse matrix just once.

## This function provides caching for the inverse of the matrix x, by using helper methods
makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    
    set <- function(y)
    {
        # use new matrix and reset the inverse matrix to null.
        x <<- y
        inverse <<- NULL
    }
    
    get <- function()
    {
        x
    }
    
    setcache <- function(m)
    {
        inverse <<- m
    }
    
    getcache <- function()
    {
        inverse
    }
    
    list(set = set, get = get, setcache = setcache, getcache  = getcache)
}


## This method returns inverse of the matrix. If inverse exists in the cache, 
## it returns cached value 
cacheSolve <- function(x, ...) 
{
    # try to get inverse from cache
    inverse <- x$getcache()
    
    if(!is.null(inverse))
    {
        # inverse is already calculated and cached
        return(inverse)   
    }
    
    # no cache. Get the matrix and calculate inverse
    data <- x$get()
    
    inverse <- solve(data, ...)
    
    # set cache
    x$setcache(inverse)
    
    inverse
}
