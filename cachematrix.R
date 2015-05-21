## The two functions implemented below are used co-operatively to define a given matrix, which should be square,and
## compute and return the inverse of that matrix
##
## If the matrix inverse has been previously computed and cached then that inverse result will be 
## returned without repeatedly and unnecessarily re-computing, thereby saving time

## The function 'makeCacheMatrix' specified below  creates a 'special' matrix comprising a list
## of named functions that will perform the following operations on the given matrix:
##
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##
## and are used within 'cacheSolve' as is appropriate

makeCacheMatrix <- function(x = matrix()) {
        
        ## specify the functions associated with the matrix
        
        set    = function(y) {
                
                ## use `<<-` assignment operator to assign values to objects 
                ## outside this function in a parent environment
                x   <<- y
                inv <<- NULL
        }
        get    = function() x
        
        setinv = function(inverse){ 
                 inv <<- inverse ## use `<<-` to assign value to an object
                                 ## defined in a parent environment
        }
        
        getinv = function() inv
        
        ## return a list of functions associated with the matrix object
        
        return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}


## The following 'cacheSolve' function performs the actual inversion computation if neccessary
## on the matrix created by the 'makeCacheMatrix' function

## First we check to see if the inverse has already been calculated, and if so, return
## the pre-computed inverse from cache. Otherwise compute the inverse, and set the result 
## in the cache variable using the setinverse function for re-cycling before returning 
## the computed inversion result

cacheSolve <- function(x, ...) {
        
        ## argument x is the output matrix from makeCacheMatrix() providing access to 
        ## the required functions
        
        invMatrix = x$getinv() ## use the 'get' function to retrieve the given matrix
                               ## from the external environment
        
        ## test if the inverse has already been calculated
        
        if (!is.null(invMatrix)){
                
                # no need to compute, so get it from the cache and return
                
                message("No need to compute inverse so retrieving pre-computed cached matrix")
                return(invMatrix)
        }
        else {
                
                # it is necessary to compute the inverse
                
                message("New matrix values so computing matrix inverse and storing in cache for future use")
                matrixData = x$get()
                invMatrix  = solve(matrixData, ...)
                
                # set the cache value of the inverse
                
                x$setinv(invMatrix)
        }
        return(invMatrix) ## return the re-computed inverse
}


## Testing scenario results follow:

# m <- makeCacheMatrix()
# m$get()
#    [,1]
# [1,] NA
#
# m$set(matrix(c(0,2,2,0),2,2))
# m$get()
# [,1] [,2]
# [1,]    0    2
# [2,]    2    0
#
# cacheSolve(m)
# New matrix values so computing matrix inverse and storing in cache for future use
# [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0

# cacheSolve(m)
# No need to compute inverse so retrieving pre-computed cached matrix
# [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0

# Change to matrix triggers fresh inversion solution:
# m$set(matrix(c(100,2,2,100),2,2))
# m$get()
# [,1] [,2]
# [1,]  100    2
# [2,]    2  100

# cacheSolve(m)
# New matrix values so computing matrix inverse and storing in cache for future use
# [,1]        [,2]
# [1,]  0.01000400 -0.00020008
# [2,] -0.00020008  0.01000400
#
# cacheSolve(m)
#No need to compute inverse so retrieving pre-computed cached matrix
# [,1]        [,2]
# [1,]  0.01000400 -0.00020008
#[2,] -0.00020008  0.01000400
#
