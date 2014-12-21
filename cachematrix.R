##makeCacheMatrix is used to cache both the Input matrix and the inverse of it


 makeCacheMatrix <- function(x = matrix()) {         
    m<-NULL              #Sets value of m to NULL(If cacheSolve not yet called upon)
    set<-function(y){    #Sets Value of Input Matrix
    x<<-y                #Caches the Input Matrix   
    m<<-NULL             # Sets value of Inverse to NULL(if cacheSolve used)      
  }
  get<-function() x      #getsvalue of Input matrix 
  setmatrix<-function(solve) m<<- solve     #setmatrix function to compute the inverse of Matrix#
  getmatrix<-function() m        #getmatrix function to cache the inverse of Matrix 
  list(set=set, get=get,          #Creates a list to house the four functions   
     setmatrix=setmatrix,
     getmatrix=getmatrix)
  }
## cacheSolve calls functions stored in matrix returned by makeCacheMatrix.If the inverse has already been calculated then cacheSolve retrieves the inverse from the cache. If not it calculates the inverse of the data and sets the inverse in the cache via the setinverse function. 
  cacheSolve <- function(x=matrix(), ...) {  #Returns inverse of matrix
      m<-x$getmatrix()              #Gets the value if an inverse has been created before
      if(!is.null(m)){              #Checks if cacheSolve has been run before    
        message("getting cached data")
        return(m)
      }
  matrix<-x$get()                  #Runs the get function and gets value of Input Matrix   
  m<-solve(matrix, ...)            #Computes the value of Inverse of Matrix
      x$setmatrix(m)               #Runs the setmatrix function to cache the Inverse Matrix   
      m                            #Returns the inverse
  }

