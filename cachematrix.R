## Below are two functions that are used to create a special object that stores a "matrix" and cache's its inverse.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   
    
    i <- NULL
  
    # set the value of the vector  
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
   
    # get the value of the vector
    
     get <- function() {
        x
    }
    
    # set the value of the mean
     
    setinverse <- function(inverse){
        i <<- inverse
    }
    
    # get the value of the mean
    
    getinverse <- function() {
        i
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The following function calculates the inverse but, it first checks to see 
## if the inverse has already been calculated and is in cache then it will return it from chache
## else, it calculates inverse and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    
    ##Checking inverse in cache.
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    
    ##finding inverse and storing it in "i".
    
    i <- solve(data, ...)
    x$setinverse(i)
    
    ## Return a matrix "i" that is the inverse of 'x'
    i
}
