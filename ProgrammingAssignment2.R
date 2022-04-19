### Programming Assignment 2: Lexical Scoping.

################################################################################

## First example

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

# First function

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  set_m_inv <- function(invertir) m_inv <<- invertir
  get_m_inv <- function() m_inv
  list(set = set, get = get,
       set_m_inv = set_m_inv,
       get_m_inv = get_m_inv)
}


################################################################################

## Second example

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

# Second function

cacheSolve <- function(x, ...) {
  m_inv <- x$get_m_inv()
  if(!is.null(m_inv)) m_inv
  matrizOK <- solve(x$get())
  matrizOK
}


