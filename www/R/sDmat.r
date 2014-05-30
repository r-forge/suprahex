#' Function to calculate distance matrix in high-dimensional input space but according to neighborhood relationships in 2D output space
#'
#' \code{sDmat} is supposed to calculate distance (measured in high-dimensional input space) to neighbors (defined by based on 2D output space) for each of hexagons/rectangles
#'
#' @param sMap an object of class "sMap"
#' @param which_neigh which neighbors in 2D output space are used for the calculation. By default, it sets to "1" for direct neighbors, and "2" for neighbors within neighbors no more than 2, and so on
#' @param distMeasure distance measure used to calculate distances in high-dimensional input space
#' 
#' @return 
#' \itemize{
#'  \item{\code{dMat}: a vector with the length of nHex. It stores the distance a hexaon/rectangle is away from its output-space-defined neighbors in high-dimensional input space}
#' }
#' @note "which_neigh" is defined in output 2D space, but "distMeasure" is defined in high-dimensional input space
#' @export
#' @seealso \code{\link{sNeighAny}}
#' @include sDmat.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#'
#' # 2) get trained using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) calculate "median" distances in INPUT space to different neighbors in 2D OUTPUT space
#' # 3a) using direct neighbors in 2D OUTPUT space
#' dMat <- sDmat(sMap=sMap, which_neigh=1, distMeasure="median")
#' # 3b) using no more than 2-topological neighbors in 2D OUTPUT space
#' # dMat <- sDmat(sMap=sMap, which_neigh=2, distMeasure="median")

sDmat <- function(sMap, which_neigh=1, distMeasure=c("median","mean","min","max"))
{
    
    distMeasure <- match.arg(distMeasure)
    
    if (class(sMap) != "sMap"){
        stop("The funciton must apply to 'sMap' object.\n")
    }
    nHex <- sMap$nHex
    M <- sMap$codebook
    if (is.vector(M)){
        M <- matrix(M, nrow=1, ncol=length(M))
    }
    
    ## calculate connections matrix between any pair of hexagons/rectangles in a grid
    aNeigh <- sNeighAny(sObj=sMap)
    
    ## only those within which_neigh but not self-self
    Ne <- (aNeigh <= which_neigh & aNeigh != 0)
    
    # distances
    dMat <- matrix(0, nrow=nHex, ncol=1)
    for(i in 1:nHex){
        ne <- which(Ne[i,])
        
        if(length(ne) >= 1){
            
            res <- sBMH(sMap=M[ne,], data=M[i,], which_bmh="all")
            
            dMat[i] <- apply(res$qerr, 1, distMeasure)
            if(0){
            if(distMeasure == "mean"){
                dMat[i] <- mean(res$qerr)
            }else if(distMeasure == "median"){
                dMat[i] <- median(res$qerr)
            }else if(distMeasure == "min"){
                dMat[i] <- min(res$qerr)
            }else if(distMeasure == "max"){
                dMat[i] <- max(res$qerr)
            }
            }
        }else{
            dMat[i] <- NA
        }
    }
    
    invisible(dMat)
}