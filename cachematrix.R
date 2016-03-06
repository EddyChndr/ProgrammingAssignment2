## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix takes matrix function as its input argument.

## This function is comprised of 4 other functions. They are set, get, setinverse, and getinverse

## set function takes matrix function as its input argument.
## This is used to replace the old matrix with the new one (to change the matrix), 
## as specified in its input.

## get function will return the matrix.

## setinverse function takes matrix function to specifiy the inverse of the matrix. 
## setinverse doesn't calculate the inverse of the matrix given in the input argument
## of either makeChacheMatrix function or set function. 

## getinverse function will return the inverse of the matrix as specified in the 
## setinverse function or cacheSolve function.


makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function (j) {
                x <<- j
                n <<- NULL
        }
        get <- function () x
        setinverse <- function (inverse = matrix()) n <<- inverse
        getinverse <- function () n 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve function will return the inverse of the matrix from the makeCacheMatrix
## function if the inverse has been calculated beforehand (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache. 

## If the inverse hasn't been calculated, then cacheSolve will calculate the inverse
## of the matrix and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n) 
        }
        data <- x$get()
        n <- solve(data)
        x$setinverse(n)
        n
}

