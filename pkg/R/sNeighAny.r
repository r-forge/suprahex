#' Function to calculate any neighbors for each hexagon/rectangle in a grid
#'
#' \code{sNeighAny} is supposed to calculate any neighbors for each hexagon/rectangle in a regular 2D grid. It returns a matrix with rows for the self, and columns for its any neighbors. 
#'
#' @param sObj an object of class "sTopol" or "sInit" or "sMap"
#' @return 
#' \itemize{
#'  \item{\code{aNeigh}: a matrix of nHex x nHex, containing distance info in terms of any neighbors, where nHex is the total number of hexagons/rectanges in the grid}
#' }
#' @note The return matrix has rows for the self, and columns for its neighbors. The non-zeros mean the distance away from its neighbors, and the zeros for the self-self. It has rows/columns ordered in the same order as the "coord" matrix of the input object does.
#' @export
#' @seealso \code{\link{sNeighDirect}}
#' @include sNeighAny.r
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
#' # 4) calculate any neighbors based on different objects
#' # 4a) based on an object of class "sTopol"
#' aNeigh <- sNeighAny(sObj=sTopol) 
#' # 4b) based on an object of class "sMap"
#' # aNeigh <- sNeighAny(sObj=sI)


sNeighAny <- function(sObj)
{

    if (!is(sObj,"sTopol") & !is(sObj,"sInit") & !is(sObj,"sMap")){
        stop("The funciton must apply to either 'sTopol' or 'sInit' or 'sMap' object.\n")
    }
    
    nHex <- sObj$nHex
    
    ## calculate direct neighbors
    dNeigh <- sNeighDirect(sObj)
    
    ######################################
    ### very important: add back self-self
    diag(dNeigh) <- 1
    ######################################    
    
    aNeigh <- dNeigh
    tNeigh <- dNeigh
    k <- 2
    while(sum(aNeigh == 0) > 0){
    	tmp <- tNeigh %*% dNeigh > 0 
        aNeigh[(tmp-tNeigh) > 0] <- k
        tNeigh <- tmp
        k <- k+1
    }
    
    ## rows for the self, cols for its any neighbors
    aNeigh <- aNeigh * (!diag(nHex)) ## excluding self-self
    
    invisible(aNeigh)
}