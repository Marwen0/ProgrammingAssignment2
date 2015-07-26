## These functions allow us to calculate the inverse of a matrix .If the inverse
## has already been calculated , then it will be returned directly .
##The calculation will only occur if the matrix is used for the first time .

## makeCacheMatrix allow us to create a list of functions that will be used to :
##get the value of the matrix / set the value of the matrix /get the value of  
##the Inverse/set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        IN = NULL
        
        get = function() x 
        
        set = function(y=matrix()) {
              x <<- y 
              IN <<- NULL }
        
        getInverse = function() IN
        
        setInverse = function(new) IN <<- new 
        
        list(get=get,set=set,getInverse=getInverse,setInverse=setInverse)
        
}


## cachesolve returns the inverse if it has already been calculated , otherwise
## it calculates it .

cacheSolve <- function(x, ...) {
        
        IN = x$getInverse() 
        
        if(!is.null(IN)) {
                message("getting cached data")
                return (IN )}
        
        m = x$get()
        new = solve(m)
        x$setInverse(new)
        new
        
}
