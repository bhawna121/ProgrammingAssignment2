##the overall code returns the inverse matrix of an iversible matrix .if the inverse has already been calculated then instead of computing again it can be cached.

#IN ths code the concepts of lexical scoping has been explained.

##there are 2 functions one that is create a special matrix and  the other function "CacheSolve" compute the inverse.

##the first function "makeCacheMAtrix" create a special matrix,uses functions like set matrix,get matrix,set inverse and get inverse...

makeCacheMatrix <- function(x = matrix()) 
{

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



##this function finds an inverse of a matrix using predefined function "solve".if the inverse has been calculated then function caches the already computed value

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'

	m<-x$getinverse()
	if(!is.null(m))
	{
	    print("getting cached data")
	    return(m)
	 }
				    
	  data<-x$get()
	  m<-solve(data,...)
	  x$setinverse(m)
	  m
}
