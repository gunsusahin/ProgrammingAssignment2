## Below are the functions to facilitate the computation of a time-consuming 
##computation of matrix inversion.


## makeCacheMatrix aims to create a special matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        g<- NULL 
set <- function(c) {
        x <<- c
        g <<- NULL 
}
get <- function() x 
setsolve <- function(solve) g <<- solve
getsolve <- function() g 
list( set = set, get = get, setsolve = setsolve, getsolve = getsolve)
 }


## cacheSolve function gives the inverse of the matrix created with makeCacheMatrix
## above. If it has already been calculated it will retrive the cached value and if not
## it will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        g <- x$getsolve()
        if(!is.null(g)) {
                message("getting the cached data")
                return(g)  
                
        }
               
        data <- x$get()
        g <- solve(data, ...)
        x$setsolve(g)
        g
        
}
