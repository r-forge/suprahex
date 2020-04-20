#' Function to calculate direct neighbors for each hexagon/rectangle in a grid
#'
#' \code{sNeighDirect} is supposed to calculate direct neighbors for each hexagon/rectangle in a regular 2D grid. It returns a matrix with rows for the self, and columns for its direct neighbors. 
#'
#' @param sObj an object of class "sTopol" or "sInit" or "sMap"
#' @return 
#' \itemize{
#'  \item{\code{dNeigh}: a matrix of nHex x nHex, containing presence/absence info in terms of direct neighbors, where nHex is the total number of hexagons/rectanges in the grid}
#' }
#' @note The return matrix has rows for the self, and columns for its direct neighbors. The "1" means the presence of direct neighbors, "0" for the absence. It has rows/columns ordered in the same order as the "coord" matrix of the input object does.
#' @export
#' @seealso \code{\link{sHexDist}}
#' @include sNeighDirect.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#'
#' # 2) from this input matrix, determine nHex=5*sqrt(nrow(data))=50, 
#' # but it returns nHex=61, via "sHexGrid(nHex=50)", to make sure a supra-hexagonal grid
#' sTopol <- sTopology(data=data, lattice="hexa", shape="suprahex") 
#'
#' # 3) initialise the codebook matrix using "uniform" method
#' sI <- sInitial(data=data, sTopol=sTopol, init="uniform") 
#'
#' # 4) calculate direct neighbors based on different objects
#' # 4a) based on an object of class "sTopol"
#' dNeigh <- sNeighDirect(sObj=sTopol)
#' # 4b) based on an object of class "sMap"
#' # dNeigh <- sNeighDirect(sObj=sI)

sNeighDirect <- function(sObj)
{

	if (!is(sObj,"sTopol") & !is(sObj,"sInit") & !is(sObj,"sMap")){
        stop("The funciton must apply to either 'sTopol' or 'sInit' or 'sMap' object.\n")
    }
    
    nHex <- sObj$nHex
    
    ## distances between between hexagons/rectangles in a grid
    ## direct neighborhood are those having the exact distance of 1
    dist <- sHexDist(sObj)
    
    ## rows for the self, cols for its direct neighbors
    dNeigh <- matrix(0, nrow=nHex, ncol=nHex)
    for(i in 1:nHex){
        inds <- which(dist[i,] < 1.001 & dist[i,] > 0) ## allow for rounding error
        dNeigh[i,inds] <- 1
    }
    
    invisible(dNeigh)
}
