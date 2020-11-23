#' Function to overlay additional data onto the trained map for viewing the distribution of that additional data
#'
#' \code{sMapOverlay} is supposed to overlay additional data onto the trained map for viewing the distribution of that additional data. It returns an object of class "sMap". It is realised by first estimating the hit histogram weighted by the neighborhood kernel, and then calculating the distribution of the additional data over the map (similarly weighted by the neighborhood kernel). The final overlaid distribution of additional data is normalised by the hit histogram.
#'
#' @param sMap an object of class "sMap"
#' @param data a data frame or matrix of input data or NULL
#' @param additional a numeric vector or numeric matrix used to overlay onto the trained map. It must have the length (if being vector) or row number (if matrix) being equal to the number of rows in input data
#' @return 
#' an object of class "sMap", a list with following components:
#' \itemize{
#'  \item{\code{nHex}: the total number of hexagons/rectanges in the grid}
#'  \item{\code{xdim}: x-dimension of the grid}
#'  \item{\code{ydim}: y-dimension of the grid}
#'  \item{\code{r}: the hypothetical radius of the grid}
#'  \item{\code{lattice}: the grid lattice}
#'  \item{\code{shape}: the grid shape}
#'  \item{\code{coord}: a matrix of nHex x 2, with rows corresponding to the coordinates of all hexagons/rectangles in the 2D map grid}
#'  \item{\code{ig}: the igraph object}
#'  \item{\code{polygon}: a tibble of 7 columns ('x','y','index','node','edge','stepCentroid','angleCentroid') storing polygon location per hexagon}
#'  \item{\code{init}: an initialisation method}
#'  \item{\code{neighKernel}: the training neighborhood kernel}
#'  \item{\code{codebook}: a codebook matrix of nHex x ncol(additional), with rows corresponding to overlaid vectors}
#'  \item{\code{hits}: a vector of nHex, each element meaning that a hexagon/rectangle contains the number of input data vectors being hit wherein}
#'  \item{\code{mqe}: the mean quantization error for the "best" BMH}
#'  \item{\code{data}: an input data matrix}
#'  \item{\code{response}: a tibble of 3 columns ('did' for rownames of input data matrix, 'index', and 'qerr' (quantization error; the distance to the "best" BMH))}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note Weighting by neighbor kernel is to avoid rigid overlaying by only focusing on the best-matching map nodes as there may exist several closest best-matching nodes for an input data vector.
#' @export
#' @seealso \code{\link{sPipeline}}, \code{\link{sBMH}}, \code{\link{sHexDist}}, \code{\link{visHexMulComp}}
#' @include sMapOverlay.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
#' colnames(data) <- paste(rep('S',10), seq(1:10), sep="")
#'
#' # 2) get trained using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) overlay additional data onto the trained map
#' # here using the first two columns of the input "data" as "additional"
#' # codebook in "sOverlay" is the same as the first two columns of codebook in "sMap"
#' sOverlay <- sMapOverlay(sMap=sMap, data=data, additional=data[,1:2])
#' 
#' # 4) viewing the distribution of that additional data
#' visHexMulComp(sOverlay)

sMapOverlay <- function(sMap, data=NULL, additional)
{
    
    ## checking sMap
    if (!is(sMap,"sMap")){
        stop("The funciton must apply to 'sMap' object.\n")
    }
    neighKernel <- sMap$neighKernel
    nHex <- sMap$nHex
	
	if(is.null(data)){
		data <- sMap$data
	}else{
		## checking data    
		if (is.vector(data)){
			data <- matrix(data, nrow=1, ncol=length(data))
		}else if(is.matrix(data) | is.data.frame(data)){
			data <- as.matrix(data)
		}
	}
    dlen <- nrow(data)

    ## checking additional    
    failed <- FALSE
    if (is.vector(additional)){
        if(length(additional)==dlen){
            additional <- matrix(additional, nrow=length(additional), ncol=1)
        }else{
            failed <- TRUE
        }
    }else if(is.matrix(additional) | is.data.frame(additional)){
        if(nrow(additional)==dlen){
            additional <- as.matrix(additional)
        }else{
            failed <- TRUE
        }
    }else if(is.null(additional)){
        failed <- TRUE
    }
    if(failed){
        stop("The input 'additional' must have the same rows/length as the input 'data'.\n")
    }
    if(!is.numeric(additional) | sum(is.na(additional))>0){
        stop("The input 'additional' must have only numeric values.\n")
    }
    
    ##################################################
    ## distances between hexagons/rectangles in a grid
    Ud <- sHexDist(sObj=sMap)
    Ud <- Ud^2 ## squared Ud (see notes radius below)
    
    ## identify the best-matching hexagons/rectangles (BMH) for the input data
    radius <- 1 ## always 1
    radius <- radius^2
    response <- sBMH(sMap=sMap, data=data, which_bmh="best")
    df_response <- tibble::tibble(did=rownames(data), index=as.vector(response$bmh), qerr=as.vector(response$qerr))
    bmh <- response$bmh
    hits <- sapply(seq(1,sMap$nHex), function(x) sum(response$bmh==x))

    ## neighborhood kernel and radius
    ## notice: Ud and radius have been squared
    if(neighKernel == "bubble"){
        H <- (Ud <= radius)
    }else if(neighKernel == "gaussian"){
        H <- exp(-Ud/(2*radius))
    }else if(neighKernel == "cutgaussian"){
        H <- exp(-Ud/(2*radius)) * (Ud <= radius)
    }else if(neighKernel == "ep"){
        H <- (1-Ud/radius) * (Ud <= radius)
    }else if(neighKernel == "gamma"){
        H <- 1/gamma(Ud/(4*radius) +1+1)
    }
    Hi <- H[,bmh] # nHex X dlen to store the prob. of each data hitting the map

    ## overlay additional to the trained sMap
    ## It already takes into account the distribution of the input data in the trained sMap
    addtional_hits <- Hi %*% additional
    data_hits <- H %*% hits
    new <- matrix(0, nrow=nHex, ncol=ncol(additional))
    cnames <- colnames(additional)
    if(is.null(cnames)){
        cnames <- seq(1,ncol(additional))
    }
    colnames(new) <- cnames
    for(i in 1:ncol(additional)){
        new[,i] <- addtional_hits[,i]/data_hits
    }

    ######################################################################################
    
    sOverlay <- list(  nHex = sMap$nHex, 
                   xdim = sMap$xdim, 
                   ydim = sMap$ydim,
                   r = sMap$r,
                   lattice = sMap$lattice,
                   shape = sMap$shape,
                   coord = sMap$coord,
                   ig = sMap$ig,
                   polygon = sMap$polygon,
                   init = sMap$init,
                   neighKernel = sMap$neighKernel,
                   codebook = new,
                   hits = hits,
                   mqe = response$mqe,
                   data = data,
                   response = df_response,
                   call = match.call(),
                   method = "suprahex")
    
    class(sOverlay) <- "sMap"
    
    sOverlay
    
}