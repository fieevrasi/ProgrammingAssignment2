## makeCacheMatrix function creates a special matrix object which can cache its inverse.
## cacheSolve computes the inverse of the matrix returned by a makeCacheMatrix function.
## Computing the inverse is done with the solve function.
## Error handling is not implemented, functions assume that matris is always invertible.
## You can test functions by following code:
## 
## 1. Create a test matrix 
## y <- makeCacheMatrix(rbind(c(1, 1/2), c(1/2, 1))) 
##
## 2. Get matrix
## y$get()
##
## Expected result:
##      [,1] [,2]
##[1,]  1.0  0.5
##[2,]  0.5  1.0
##
## 3. Compute the inverse and cache it
## cacheinverse(y)
##
## Expected result:
##		[,1]       [,2]
##[1,]  1.3333333 -0.6666667
##[2,] -0.6666667  1.3333333
##
## 4. Get inverse 
## y$getinverse()
##
## Expected result:
## Same as above (Step 3)
##
## 5. Compute the inverse again (now it should be retrieved from the cache)
## cacheinverse(y)
##
## Expected result:
## getting cached data
##      [,1]       [,2]
##[1,]  1.3333333 -0.6666667
##[2,] -0.6666667  1.3333333
##
## 6. Change matrix
## y$set(rbind(c(1/2, 1), c(1, 1/2)))
##
## 7. Compute the inverse
## cacheinverse(y)
##
## Expected result:
##   [,1]       [,2]
##[1,] -0.6666667  1.3333333
##[2,]  1.3333333 -0.6666667
##
##



Computing the inverse of a square matrix can be done with the `solve`
function in R. For example, if `X` is a square invertible matrix, then
`solve(X)` returns its inverse.

For this assignment, assume that the matrix supplied is always
invertible.

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the matrix
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve function will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
