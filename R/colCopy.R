
# colCopy() binds multiple copies of a vector together, each as a separate column.
colCopy <- function(x,times, dimnames=NULL ) {
  if( !is.vector(x) ) stop( '"x" must be a vector')
  if( length(times) !=1 | !methods::is(times,"numeric")) stop( '"times" must be a single number')
  if( is.null(dimnames) ) dimnames<-list(names(x),character(0))
  matrix( x, length(x), times, byrow=FALSE, dimnames )

  # alternative code:
  # replicate(times,x)
}

