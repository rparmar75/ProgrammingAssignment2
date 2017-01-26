##
## ProgrammingAssignment2
## 
## 1.makeCacheMatrix: This function creates an object of matrix class that can 
## cache the inverse of the matrix.
## 
## 2.cacheSolve: This function computes the inverse of the matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated and the
## underlying matrix object has not changed then cached inverse is returned


## The following function takes a matrix as an input and returns a list of
## functions. The matrix is assumed to be invertible and hence the function
## doesn't check for non-singularity of the mtarix.

makeCacheMatrix <- function(x = matrix()) {

    inverse <<- NULL
    
    # Good to have the following "set" function corresponding to "get". However,
    # this function is not needed really for this assignent as cacheSolve 
    # doesn't need to invoke the "set" function as such. Instead of resetting
    # the matrix by calling "set", we can just call makeCacheMatrix again with
    # the new matrix passed as the arument.
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    
    
}



## The following function returns the inverse of the matrix, retrieved by the 
## "getinverse" function if cached. If the inverse is not cached by "x" then the
## inverse is calculated and cached. Also, if the underying matrix has
## changed then it is recalculated.


cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("returning cached inverse of the matrix")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...) # solve(base) function used to calculate inverse 
    x$setinverse(inverse)
    inverse    
    
    
}
