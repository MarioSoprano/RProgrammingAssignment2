## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matriz <- x$get()
  inv <- solve(matriz, ...)
  x$setinverse(inv)
  inv
}

##Demostration:

#First: get the matrix (c(3,2,4,6), 2, 2) 

matrix0 <- makeCacheMatrix(matrix(c(3,2,4,6), 2, 2))
matrix1 <- matrix0$get()#Get the matrix (c(3,2,4,6), 2, 2)

#output

#      [,1] [,2]
#[1,]    3    4
#[2,]    2    6

#Second: check the inverse matrix doesn´t exist

matrix0$getinverse()

#output

#NULL 

#Third: get the inverse matrix 

matrix2 <- cacheSolve(matrix0)

#output

#      [,1] [,2]
#[1,]  0.6 -0.4
#[2,] -0.2  0.3

#Fourth: check if the function retrieves the inverse from the cache

cacheSolve(matrix0)

#output

#getting cached data
#     [,1] [,2]
#[1,]  0.6 -0.4
#[2,] -0.2  0.3

#Fifth: multiply both matrixes to obtain the identity matrix to check the correct result 

matrix3 <- matrix1%*%matrix2

#output

#      [,1] [,2]
#[1,]    1    0
#[2,]    0    1
