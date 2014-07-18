## The 'makeCacheMatrix' function creates a list that consists of functions to 
## set the value of a matrix and the caculated value of an inverse matrix
## into a cache; and to get the cached value of a matrix and the caculated 
## result out. The 'cacheSolve' function is to compute the inverse of the matrix
## cached by the previous function.

## To create a list of functions: set(), get(), setSolve(), getSolve()
makeCacheMatrix <- function(x = matrix()) 
{
        I <- NULL ## variable for caching an Inverse
        set <- function(y)
        {
                x <<- y ## To assign the input value to 'x' defined 
                        ## at the parent environment
                I <<- NULL
        }
        get <- function()
        {
                x
        }
        setSolve <- function(Solve)
        {
                I <<- Solve ## To cache an inverse
        }
        getSolve <- function()
        {
                I
        }
        list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


## To compute the inverse of a matrix. If it has already been computed and cached,
## the function will return that value.
cacheSolve <- function(x, ...)
{
        I <- x$getSolve()
        if(!is.null(I)) ## To check whether there is the caculated value or not
        {
                message("getting cached data")
                return(I)
        }
        data <- x$get() 
        I <- solve(data, ...) ## if not, the 'solve' function will compute the inverse
        x$setSolve(I) ## To cache the inverse
        I ## To return a matrix that is the inverse of 'x'
}
