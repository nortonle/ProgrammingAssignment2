## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  ix <- NULL
  get <- function()
  {
    x
  }
  set <- function(y = matrix())
  {
    x <<- y
    ix <<- NULL
  }
  getInverse <- function()
  {
    ix
  }
  setInverse <- function(temp = matrix())
  {
    ix <<- temp
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  ix <- x$getInverse()
  if (!is.null(ix))
  {
    message("getting cached data")
    return(ix)
  }
  
  v_data <- x$get()
  ix <- solve(v_data)
  x$setInverse(ix)
  return(ix)
}
