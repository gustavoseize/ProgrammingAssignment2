## The idea is to cache calculated inverses of matrixes
## makeCacheMatrix function will store a special vector with the calculation 
##      and list function to get and set values of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_mtx_inverse <- function(solve) m <<- solve
  get_mtx_inverse <- function() m
  list(set = set, get = get,
       set_mtx_inverse = set_mtx_inverse,
       get_mtx_inverse = get_mtx_inverse)
}


## CacheSolve function will calculate the inverse of the matrix on the special vector 
##   or skip if the matrix inverse has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_mtx_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_mtx_inverse(m)
  m
}

## Obs1: Solve function works wonly for inversable matrixes.
## Obs2: Clear variables first
## cacheSolve argument is the special vector and not the matrix direclty

## a <- matrix(c(4,5,7,20), nrow=2, ncol=2)
## mtx_1 <- makeCacheMatrix(a)
## cacheSolve(mtx)

## [,1]        [,2]
## [1,]  0.4444444 -0.15555556
## [2,] -0.1111111  0.08888889

## b <- matrix(c(4,5,7,20,3,1,7,9,10), nrow=3, ncol=3)
## mtx_2 <- makeCacheMatrix(b) 
## cacheSolve(mtx)
## 
## [,1]        [,2]
## [1,]  0.4444444 -0.15555556
## [2,] -0.1111111  0.08888889


