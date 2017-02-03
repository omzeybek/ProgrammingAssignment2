## STAGE 1 : Cashing the inverse of the matrix 

## First i need a matrix object to casche its inverse 
##Following function creates purposed matrix

Casche_Matrix<- function( x = matrix() ) { # created a matrix named x, following i am creating objects
                                         # which i will be using in the next steps 
inv <- NULL 
set<- function(y) {
  x <<- y
  m <<-  NULL 
}

# now i am creating  getter and setter modules
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


# following function computes the  inverse of matrix created by Casche_Matrix function
# if  the  inverse has already been calculated then it should be in line with the inverse from the Cache.

## STAGE 2 Calculating inverse matrix of Casche

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
