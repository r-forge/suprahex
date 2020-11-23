#' Function to identify local minima (in 2D output space) of distance matrix (in high-dimensional input space)
#'
#' \code{sDmatMinima} is supposed to identify local minima of distance matrix (resulting from \code{\link{sDmat}}). The criterion of being local minima is that the distance associated with a hexagon/rectangle is always smaller than its direct neighbors (i.e., 1-neighborhood)
#'
#' @param sMap an object of class "sMap"
#' @param which_neigh which neighbors in 2D output space are used for the calculation. By default, it sets to "1" for direct neighbors, and "2" for neighbors within neighbors no more than 2, and so on
#' @param distMeasure distance measure used to calculate distances in high-dimensional input space. It can be one of "median", "mean", "min" and "max" measures
#' @param constraint logic whether further constraint applied. If TRUE, only consider those hexagons 1) with 2 or more neighbors; and 2) neighbors are not within minima already found (due to the same distance)
#' 
#' @return 
#' \itemize{
#'  \item{\code{minima}: a vector to store a list of local minima (represented by the indexes of hexogans/rectangles}
#' }
#' @note Do not get confused by "which_neigh" and the criteria of being local minima. Both of them deal with 2D output space. However, "which_neigh" is used to assist in the calculation of distance matrix (so can be 1-neighborhood or more); instead, the criterion of being local minima is only 1-neighborhood in the strictest sense
#' @export
#' @seealso \code{\link{sDmat}}, \code{\link{sNeighAny}}
#' @include sDmatMinima.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#'
#' # 2) get trained using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) identify local minima of distance matrix based on "median" distances and direct neighbors
#' minima <- sDmatMinima(sMap=sMap, which_neigh=1, distMeasure="median")

sDmatMinima <- function(sMap, which_neigh=1, distMeasure=c("median","mean","min","max"), constraint=TRUE)
{
    
    distMeasure <- match.arg(distMeasure)
    
    if (!is(sMap, "sMap")){
        stop("The funciton must apply to 'sMap' object.\n")
    }
    nHex <- sMap$nHex
    
    # Find clusters based on local minima of distance matrix
    
    ## calculate distance (defined by "distMeasure" in INPUT space) to neighbors (defined by "which_neight" in 2D OUTPUT space) for each of hexagons/rectangles according to the codebook matrix and the definition of neighbors
    dMat <- sDmat(sMap=sMap, which_neigh=which_neigh, distMeasure=distMeasure)

    ## calculate connections matrix between any pair of hexagons/rectangles in a grid
    if(which_neigh==1){
    	aNeigh <- sNeighDirect(sObj=sMap)
    }else{
    	aNeigh <- sNeighAny(sObj=sMap)
    }
    
    ## find local minima (always 1-neighborhood)
    minima <- vector()
    k <- 0
    for(i in 1:nHex){
        
		ne <- which(aNeigh[i,] == 1) ## should be only 1-neighborhood
		#ne <- which(aNeigh[i,] <= which_neigh & Ne[i,] > 0)
		
		if(constraint){
			# only those with 2 or more neighbors
			# neighbors are not within minima already found (due to the same distance)
			if(length(ne)<=1 | any(ne %in% minima)){
				next
			}
		}

		tmpSum <- sum(dMat[ne] >= dMat[i])
		if(tmpSum == length(ne)){
			k <- k+1
			minima[k] <- i
		}

    }
    minima <- unique(minima)
    
    invisible(minima)
}