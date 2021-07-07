## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #create a matrix
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x          #get the created matrix
        #set the inverse of the matrix
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m  #get the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){         #get the inverse of a matrix if found in cache
                message("gettimg cached data")
                return(m)
        } 
        data <- x$get()          #get a matrix
        m <- solve(data, ...)    #get inverse of a matrix if it wasn't in cache
        x$setinverse(m)
        m                        #return the inverse
}
