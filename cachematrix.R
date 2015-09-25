# GENERAL COMMENT
#The purpose of these functions is to store some data on cache in order to use them instead of estimate
#them everytime that the same data is needed.In this case the functions have to find the inverse
#of a matrix.We assume that every matrix  provided will be invertible.

#MAKECACHEMATRIX COMMENT
#This function stores a list of four functions.When we
#assign makeCacheMatrix to an object it will have all
#the four functions.

makeCacheMatrix <- function(x=matrix()){
        i <- NULL
        set <- function(y){
                x <<-y
                i <<- NULL
        }
        
        get <-function() x
        setinverse <- function(solve) i<<-solve
        getinverse <- function()i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        
        
}


# CACHESOLVE COMMENT
#This function calculates the inverse of the matrix 
#created in the function above. But first checks if it
#has been already estimated.If that is the case, the function
#returns a message, give the inverse estimated before and 
#stop the computation; otherwise the function makes the
#the inverse and returns it.

cachesolve <- function(x, ...){
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}
