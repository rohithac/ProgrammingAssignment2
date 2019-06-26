## Function to cache the inverse of a matrix


## to create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,get = get,setinv = setinv,getinv = getinv)
}


## to compute the inverse of matrix or to retrieve if alrdy computed

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if(!is.null(inv))
  {
    message("retrieving cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
