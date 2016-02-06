## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Return a list containing function 
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## reset the inverse
        set <- function(y){
        x <<-y
        inv <<- NULL
        }
        ## get the value of x
        get <- function() x
        ## set the value of the inverse
        setinv <- function(inver) inv <<- inver
        ## get the value of the inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
}


## Write a short comment describing this function
## Calculate the inverse of matrix created by the previous function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## if the inverse is already calculated, it will retrieve from the cache.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if the inverse is not calculated, use solve() to calculate
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
