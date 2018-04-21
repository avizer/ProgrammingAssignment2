## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
m<- NULL
set <- function(y) {
    x<<-y
    m<<-NULL
}
get <-function()x
    setsolve<- function(solve) m<<- solve
    getsolve<-function() m
    list(set=set, get=get,
         setsolve=setsolve,
         getsolve=getsolve)
}


## Computes inverse of "matrix" object from makeCacheMatrix function.
##If inverse already calculated and matrix unchanged, retrieve inverse from cache

cacheSolve <- function(x=matrix(),...) {
    m<-x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix,...)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
