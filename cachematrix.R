---
title: "cachematrix code"
author: "David F George"
date: "Tuesday, May 19, 2015"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```
## The following two functions are used to calculate the inverse of a matrix, 
## if the matrix has been previously cached then the inverse of a matrix will be 
## returned without repeatedly computing a result

## The function 'makeCacheMatrix'
## creates a special matrixù, which is really a listcontainina 
## function that will:

##    set the value of the matrix
##    get the value of the matrix
##    set the inverse of the matrix
##    get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
        ## the input variable 'x' is a square invertible matrix
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ## this list is used as the input to cacheSolve()
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object defined in a parent environment
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        ## return: a list of functions to
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The following cacheSolve function calculates the inverse matrix of the special "matrix"
## created with the above makeCacheMatrix function.
##It will first check to see if the inverse has already been calculated. If so, it gets
##the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
##of the data and sets the value of the inverse in the cache via the 
##setinverse function.

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
```

