

# rowCopy() binds multiple copies of a vector together, each as a separate row
rowCopy <- function(x,times, dimnames=NULL ) {
  if( !is.vector(x) ) stop( '"x" must be a vector')
  if( length(times) !=1 | !methods::is(times,"numeric")) stop( '"times" must be a single number')
  if( is.null(dimnames) ) dimnames<-list(character(0),names(x))
  matrix( x, times, length(x), byrow=TRUE, dimnames )

  # alternative code:
  # t(replicate(times,x))
}
