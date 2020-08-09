## Put comments here that give an overall description of what your
## functions do
## R programming Week 3 Assignment solutions 

## Write a short comment describing this function
## makeChacheMatrix creates a matrix that allows to store the data and its
## inverse in x. 

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y){
                    x<<- y
                    inv<<- NULL
          }
          get <- function () x
          setinv<- function(inv) inv<- inverse
          getinv<- function() inv
          list (set=set, get=get,
                setinv=setinv,
                getinv=getinv)
}


## Write a short comment describing this function
## cacheSolve function allows the matrix stored in x to be called and then
## compute the inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
          if(!is.null(inv)){
                    message("getting cached data")
                    return(inv)
          }
          data <- x$get()
          inv<- solve(data,...)
          x$setinv(inv)
          inv
}

