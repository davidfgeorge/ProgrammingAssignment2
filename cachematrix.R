## The following two functions are used to calculate the inverse of a matrix, 
## if the matrix has been previously cached then the inverse of a matrix will be 
## returned without repeatedly computing a result

## The following function 'makeCacheMatrix'
## creates a special “matrix”, which is really a list containing a 
## function that will:

##    set the value of the matrix
##    get the value of the matrix
##    set the inverse of the matrix
##    get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
x <<- y
m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse
}


## The following cacheSolve function calculates the inverse matrix of the special "matrix"
## created with the above makeCacheMatrix function.
##It will first check to see if the inverse has already been calculated. If so, it gets
##the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
##of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- solve(data)
x$setinverse(m)
m
}
}
