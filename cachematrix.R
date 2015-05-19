---
title: "Programming Assignment 2 Code in Markdown format"
author: "David F George"
date: "Tuesday, May 19, 2015"
output: html_document
---

This is an R Markdown document produced by RStudio containing the R code for functions
'makeCacheMatrix' and 'cacheSolve'

```
## The following two functions are used to compute the inverse of a matrix
##
## If the matrix has been previously computed and cached then the inverse of a matrix will be 
## returned without unnecessarily and repeatedly computing a result thereby saving time



## The function 'makeCacheMatrix' specified below
## creates a 'special' matrix comprising a list of named functions that will perform the following
## operations on a given (squared) matrix:
##
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##
## and are used within cacheSolve()as is appropriate

makeCacheMatrix <- function(x = matrix()) {
        
        ## the input variable defined by input argumant 'x' is the matrix of interest
        
        set    = function(y) {
                
                ## use `<<-` assignment operator to assign values to objects outside this function in a parent environment
                x   <<- y
                inv <<- NULL
        }
        get    = function() x
        
        setinv = function(inverse) inv <<- inverse ## use `<<-` to assign value to an object
                                                   ## defined in a parent environment
        getinv = function() inv
        
        ## return a list of functions associated with the matrix object
        
        return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}

## The following 'cacheSolve' function performs the actual inversion computation, if neccessary
## on the matrix created by the above makeCacheMatrix function

## First check to see if the inverse has already been calculated, and if so, return
## the pre-computed inverse from cache. Otherwise compute the inverse, and set the result 
## in the cache variable using the setinverse function.

cacheSolve <- function(x, ...) {
        
        ## argument x is the output matrix from makeCacheMatrix()
        
        inv = x$getinv()
        
        ## test if the inverse has already been calculated
        
        if (!is.null(inv)){
                
                # no need to compute, so get it from the cache and return
                
                message("No need to compute inverse so retrieving pre-computed cached matrix")
                return(inv)
        }
        else {
                
                # it is necessary to compute the inverse
                
                message("New matrix values so computing matrix inverse and storing in cache for future use")
                mat.data = x$get()
                inv      = solve(mat.data, ...)
                
                # set the cache value of the inverse
                
                x$setinv(inv)
        }
        return(inv) ## return the re-computed inverse
}
```


