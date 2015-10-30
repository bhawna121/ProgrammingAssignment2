## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    m<-NULL#inverse of matrix
        set<-function(y)
	    {
	            x<<-y
		            m<<-NULL
			        }
				    
				        get<-function()x
					    set_inverse<-function(reverse)m<<-reverse
					        getreverse<-function()m
						    
						        list(set=set,get=get,setinverse=set_inverse,getinverse=getreverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m<-x$getinverse()
	    if(!is.null(m)){
	            print("getting cached data")
		            return(m)
			        }
				    
				        data<-x$get()
					    m<-solve(data,...)
					        x$setinverse(m)
						    m
}
