## ProgrammingAssignment2

## Creat a special matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(Y){
        X <<- Y
        m <<- NULL
    }
    get <- function() X
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## This function return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}
