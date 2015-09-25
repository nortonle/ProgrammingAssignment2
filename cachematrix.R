##makeCacheMatrix has 4 sub functions, which are get, set, getInverse and setInverse
##get allows users to modify the orignial matrix
##get returns the orignial matrix that the function contains
##setInverse allows users to cache the original matrix's inverse matrix. Initially, this is null.
##getInverse returns the inverse matrix, if there is any.
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


##cacheSolve returns the cached inverse matrix if there is any. If there is no inverse matrix, it will solve one.
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  ix <- x$getInverse()
  if (!is.null(ix))
  {
    message("getting cached data")
    return(ix)
  }
  
  ##If the function detects there is no cached data, it begins solving inverse matrix.
  v_data <- x$get()
  ix <- solve(v_data)
  x$setInverse(ix)
  return(ix)
}
