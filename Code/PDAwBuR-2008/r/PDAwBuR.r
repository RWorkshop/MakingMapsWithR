mcmc.list.bugs <-
function( x, ... )
{
# Take output from bugs() that be a bugs object or a charcter string
# (as from bugs() calling WinBUGS with option codaPkg=TRUE)
# and turn it into a mcmc.list
if (!is.R() && !require("coda"))
    stop("package 'coda' is required to use this function")
if( is.character(x) )
   res <- mcmc.list(lapply(x,
                   read.coda,
                  index.file = file.path(dirname(x[1]),
                                         "codaIndex.txt"), ...))
if( inherits(x,"bugs") )
  {
  zz <- list(list())
  aa <- x$sims.array
  for( i in 1:(dim(aa)[2]) )
     {
     tmp <- mcmc( aa[,i,] )
     zz <- c( zz, list(tmp) )
     }
  res <- mcmc.list( zz[-1] )
  }
res
}

data.frame.mcmc.list <-
function( x, ... )
{
# Make a dataframe from an mcmc.list object
# Useful for variable manipulation
zz <- as.data.frame( as.matrix( x, chain=TRUE ) )
attr(zz,"mcpar") <- attr(x[[1]],"mcpar")
zz
}

mcmc.list.matrix <-
function( x, mcpar=c(1,nrow(x),1), ... )
{
# Make an mcmc.list object from a matrix.
# Recover the mcpar attribute if the matrix originally came from
# an mcmc.list object
if( !is.null( attr(x,"mcpar") ) ) mcpar <- attr( x, "mcpar" )
attr( zz, "mcpar" ) <- mcpar
mcmc.list.data.frame( zz )
}

mcmc.list.data.frame <-
function( x, mcpar=c(1,nrow(x),1), ... )
{
# Make an mcmc.list object from a data frame.
# Recover the mcpar attribute if the frame originally came from
# an mcmc.list object

# Check that a CHAIN colum is actually present
if( is.na( wh <- match( "CHAIN", names(x) ) ) )
    stop( "The dataframe must have a 'CHAIN' column" )

# Get the mcpar if they were there from an earlier converion
# or derive from the ITER column
if( !is.null( attr(x,"mcpar") ) ) mcpar <- attr( x, "mcpar" )
else if( "ITER" %in% names(x) ) mcpar <- c( x$ITER[1],
                                            x$ITER[nrow(x)],
                                            x$ITER[2]-x$ITER[1] )

# Utility to put attributes on components of the list
# Must be defined here, because it refers to the "mcpar" of this
# environment
mcmc.att <-
function( obj )
{
zz <- as.matrix( obj )
attr(zz,"mcpar") <- mcpar
class(zz) <- "mcmc"
return(zz)
}

# Carve the matrix in chunks and put it in a list
xx <- lapply( split( x[,-wh], x[,wh] ), mcmc.att )
class( xx ) <- "mcmc.list"
xx
}

mcmc.matrix <-
function( x )
{
# Turn a matrix into a mcmc object
}

mcmc.data.frame <-
function( x )
{
# Turn a dataframe into a mcmc object
}

mcmc.mcmc.list <-
function( x )
{
# Turn a mcmc.list obejct into a mcmc object
mcmc( as.matrix( x ) )
}
