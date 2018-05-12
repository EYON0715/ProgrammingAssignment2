## Put comments here that give an overall description of what your
## functions do (Cache the inverse of a matrix)

##Write a short comment describing this function (This function creates a matrix object which caches its inverse)

makeCacheMatrix <- function(x = matrix()) {

              m<- NULL
              set <- function(y){
                  x<<- y
                  m<<-NULL
              }
              get<-function()x
              setinverse <- function(inverse) m<<- inverse
              getinverse <- function()m
              list(set = set, get= get,
                   setinverse= setinverse
                   getinverse= getinverse)
              
}


## Write a short comment describing this function (Computes the inverse of the matrix with the above function.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the calculation
## Otherwise, it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if(!is.null(m)){
               message("getting cached data")
               return(m)
        }
        data<- x$get()
        m<- solve(data, ...)
        x$setinverse(m)
        m
}

