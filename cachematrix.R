#
# Input: a numeric square matrix
# Output: a list composed of functions to facilitate reading and storing
#         values related to the caching and computation of a matrix inverse
#
makeCacheMatrix <- function(argMatrix = matrix()) {
  
  ##
  ## The inverse of the matrix will be assigned to
  ## this variable.
  ##
  cachedMatrixInverse <- NULL
  
  ##
  ## Setter (In this assignment we don't use the set().)
  ##
  set <- function(newMatrix){
    argMatrix <<- newMatrix
    cachedMatrixInverse <<- NULL
  }
  
  ##
  ## Getter
  ##
  get <- function() {
    return(argMatrix)
  }
  
  ##
  ## argMatrix is a plain ole square matrix and
  ## we have not computed the inverse.
  ##
  setInverseMatrix <- function(newInverseMatrix) {
    cachedMatrixInverse <<- newInverseMatrix
  }
  
  ##
  ## Get the inversed cached matrix.
  ##
  getInverseMatrix <- function() {
    return(cachedMatrixInverse)
  }
  
  ##
  ## Return the makeCacheMatrix object.
  ##
  return(list(set=set, get=get,
       setInverseMatrix=setInverseMatrix,
       getInverseMatrix=getInverseMatrix))
}

#
# Input: a `cacheMatrix` list structure created by `makeCacheMatrix`
# Output: matrix, the inverse of the matrix
#
cacheSolve <- function(argCacheMatrix=makeCacheMatrix(), ...) {
  
  ##
  ## Attempt to get the inverse matrix.
  ##
  matrixInverse <- argCacheMatrix$getInverseMatrix()
  
  ##
  ## Check if we have an inverse matrix.
  ##
  if (!is.null(matrixInverse)) {
  
    ##
    ## It is the inverse matrix.
    ##
    print("We found the inverse in the cache.")
    
  } else {
    
    print("Compute and cache the inverse of the matrix.")
  
    ##
    ## Get the inverse of the matrix.
    ##
    matrix <- argCacheMatrix$get()
    
    ##
    ## Use the function solve() to get the inverse
    ## of the matrix.
    ##
    matrixInverse <- solve(matrix, ...)
    
    ##
    ## Set the inverse of the matrix.
    ##
    argCacheMatrix$setInverseMatrix(matrixInverse)      
  
  }
    
  ##
  ## Return the inverse of the matrix.
  ##
  return(matrixInverse)
}

##
## Test: Run them in this order.
#
## ℝ> a <- makeCacheMatrix(rbind(c(1,-1/4), c(-1/4,1)))
## ℝ> a$getInverseMatrix()
## NULL
## ℝ> cacheSolve(a)
## [1] "Compute and cache the inverse of the matrix."
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## ℝ> cacheSolve(a)
## [1] "We found the inverse in the cache."
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## ℝ> a$set(rbind(c(1,-1/5), c(-1/5,1)))
## ℝ> a$get()
##       [,1] [,2]
## [1,]  1.0 -0.2
## [2,] -0.2  1.0
## ℝ> a$getInverseMatrix()
## NULL
## ℝ> cacheSolve(a)
## [1] "Compute and cache the inverse of the matrix."
##           [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
## ℝ> a$getInverseMatrix()
##           [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
## ℝ> cacheSolve(a)
## [1] "We found the inverse in the cache."
## [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
##