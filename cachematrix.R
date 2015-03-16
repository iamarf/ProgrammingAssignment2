## This piece of code provides the storage of a matrix with the 
## capability of caching its inverse. This may be advantageous 
## if the inverse of the same matrix has to be reused repeteadly
## since the computation of matrix inverse may be quite 
## time-consuming

## The functionality is provided by two functions 

## The first function, `makeCacheMatrix` creates a special kind
## of "matrix", which is really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Use: a "special matrix" object is created by means of...
        ## x <- makeCacheMatrix(A)
        ## where A is a given matrix object
        
        ## inv is where the matrix inverse is cached
        inv <- NULL
        
        ## Use 1.: if a "special matrix" object 'x' is available,
        ## a new matrix 'B' can be stored in it with a call to
        ## x$set(B)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Use 2.: if a "special matrix" object 'x' is available,
        ## the matrix stored in it can be retrieved by
        ## x$get()
        get <- function() x
        
        ## Use 3.: if a "special matrix" object 'x' is available,
        ## the inverse 'inverse' can be cached in it by means of
        ## x$setinverse(inverse)
        setinverse <- function(inverse) inv <<- inverse

        ## Use 4.: if a "special matrix" object 'x' is available,
        ## the inverse 'inv' can be retrived from the cache by means of
        ## x$getinverse(inverse)
        getinverse <- function() inv
        
        ## The returned value of makeCacheMatrix, when called with...
        ## x <- makeCacheMatrix(A)
        ## is a list of the 4 previous functions...
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The second function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and puts it in the cache via the `setinverse`
## function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## I'm trying to retrieve inverse of matrix from cache...
        inv <- x$getinverse()
        ## ... getting inverse if it is there...
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        ## ... otherwise I'm going to calculate it...
        data <- x$get()
        inv <- solve(data)
        ## ...and store it in the cache
        x$setinverse(inv)
        ## Then I'm going to return inverse...
        inv
}
