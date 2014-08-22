## Put comments here that give an overall description of what your
## functions do

## Create a square matrix and cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #  Sets the matrix to be used within the scope of makeCacheMatrix
  #  Since the matrix was just set, we want to null out the inverse matrix m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Retrieve the matrix x
  get <- function() x
  
  # Set
  setInverse <- function(solve) m <<- solve
  
  # Get the inverse of the cached matrix m.  Note that if we have not called setInverse before,
  # then m will be null.
  getInverse <- function() m
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## using the cached matrix compute the inverse of the matrix.
## If the cached matrix is  null, attempt to get the inverse of it and store it for future use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Here we are attempting to get the cached value of m using getInverse()
  ## If we have not called setInverse before, then m will be null
  m <- x$getInverse()
  
  ## Check if m is null.  If it isn't then we retrive the cached value of m and return that
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If m is null, then we retrive the value of the matrix using get from the function list as part
  ## of the set of functions that passed in through x
  data <- x$get()
  
  ##  calcualte the inverse of data and store it in m
  m <- solve(data, ...)
  
  ## cache the inverse
  x$setInverse(m)
  
  ## return m
  m
}


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}



