#' Function to identify the best-matching hexagons/rectangles for the input data
#'
#' \code{sBMH} is supposed to identify the best-matching hexagons/rectangles (BMH) for the input data.
#'
#' @param sMap an object of class "sMap" or a codebook matrix
#' @param data a data frame or matrix of input data
#' @param which_bmh which BMH is requested. It can be a vector consisting of any integer values from [1, nHex]. Alternatively, it can also be one of "best", "worst" and "all" choices. Here, "best" is equivalent to \eqn{1}, "worst" for \eqn{nHex}, and "all" for \eqn{seq(1,nHex)}
#' @return 
#' a list with following components:
#' \itemize{
#'  \item{\code{bmh}: the requested BMH matrix of dlen x length(which_bmh), where dlen is the total number of rows of the input data}
#'  \item{\code{qerr}: the corresponding matrix of quantization errors (i.e., the distance between the input data and their BMH), with the same dimensions as "bmh" above}
#'  \item{\code{mqe}: the mean quantization error for the "best" BMH}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note "which_bmh" upon request can be a vector consisting of any integer values from [1, nHex]
#' @export
#' @seealso \code{\link{sPipeline}}
#' @include sBMH.r
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
#' # 4) define trainology at "rough" stage
#' sT_rough <- sTrainology(sMap=sI, data=data, stage="rough") 
#'
#' # 5) training at "rough" stage
#' sM_rough <- sTrainBatch(sMap=sI, data=data, sTrain=sT_rough)
#' 
#' # 6) define trainology at "finetune" stage
#' sT_finetune <- sTrainology(sMap=sI, data=data, stage="finetune")
#'
#' # 7) training at "finetune" stage
#' sM_finetune <- sTrainBatch(sMap=sM_rough, data=data, sTrain=sT_rough)
#'
#' # 8) find the best-matching hexagons/rectangles for the input data
#' response <- sBMH(sMap=sM_finetune, data=data, which_bmh="best")

sBMH <- function(sMap, data, which_bmh=c("best", "worst", "all"))
{
    
    if (class(sMap) == "sMap" ){
        M <- sMap$codebook
    }else{
        M <- sMap
    }
    if (is.vector(M)){
        M <- matrix(M, nrow=1, ncol=length(M))
    }
    nHex <- nrow(M)
    
    if (is.vector(data)){
        data <- matrix(data, nrow=1, ncol=length(data))
    }else if(is.matrix(data) | is.data.frame(data)){
        data <- as.matrix(data)
    }else if(is.null(data)){
        stop("The input data must be not NULL.\n")
    }
    dlen <- nrow(data)
    D <- data
    
    if(ncol(M) != ncol(D)){
        stop("The dimension (the column name) of input data must be the same as that of sMap.\n")
    }
    
    if(length(which_bmh) == 1){
        if(which_bmh == "best"){
            which_bmh <- 1
        }else if(which_bmh == "worst"){
            which_bmh <- nHex
        }else if(which_bmh == "all"){
            which_bmh <- seq(1, nHex)
        }else{
            which_bmh <- 1
        }
    }else{
        which_bmh <- as.vector(which_bmh)
    }
    
    ## BMH for each data vector, having dlen x length(which_bmh)
    bmh <- matrix(0, nrow=dlen, ncol=length(which_bmh))
    ## the corresponding quantization errors, size as bmh
    Qerr <- matrix(0, nrow=dlen, ncol=length(which_bmh))
    
    ## The BMH search involves calculating Euclidian distances to all map hexagons/rectangles for each data vector
    ## However, taking into account that distance between vectors m and v can be expressed as |m - v|^2 = sum_i ((m_i - v_i)^2) = sum_i (m_i^2 + v_i^2 - 2*m_i*v_i), this can be made much faster by transforming it to a matrix operation: Dist = (M.^2)*ones(dim,dlen) + ones(nHex,dim)*(t(D).^2) - 2*M*t(D)
    
    tmp_d <- matrix(1, nrow=ncol(M), ncol=dlen)
    tmp_m <- matrix(1, nrow=1, ncol=ncol(M))
    
    ## constant term: 1 X dlen
    const = tmp_m %*% t(D^2)
    
    ## blen: block from dlen
    blen <- min(nHex, dlen)
    i_block <- 1
    while (i_block <= dlen){
        
        inds <- seq(i_block, min(dlen,i_block+blen))
        
        ## tmp_dist has a dimension of nHex X length(inds)
        if(length(inds) == 1){
            tmp_dist <- (M^2) %*% tmp_d[,inds] - 2 * (M %*% D[inds,]) 
        }else{
            tmp_dist <- (M^2) %*% tmp_d[,inds] - 2 * (M %*% t(D[inds,])) 
        }
        
        if(length(which_bmh) == 1){
            ## for bmh
            min_ind <- apply(tmp_dist, 2, function(x) min(which(x == min(x))))
            bmh[inds,] <- min_ind
            ## for Qerr
            min_val <- apply(tmp_dist, 2, function(x) min(x))
            Qerr[inds,] <- min_val + const[1,inds]
        }else{
            for (j in 1:ncol(tmp_dist)){
                x=tmp_dist[,j]
                
                res <- sort.int(x, decreasing=F, index.return=T)
                ## x: contain the sorted numbers
                ## ix: contain the ordering index vector
                
                ## for bmh
                bmh[inds[j],] <- res$ix[which_bmh]
                ## for Qerr
                Qerr[inds[j],] <- res$x[which_bmh] + const[1,inds[j]]
                
            }
        }
        
        i_block <- i_block+blen+1
    }
    
    #Qerr[Qerr < 0] <- 0
    #Qerr <- sqrt(Qerr)
    ## mean quantization error: Average distance between each data vector and its BMH
    mqe <- mean(Qerr[!is.na(Qerr[,1]),1])
    
    ## bmh and qerr: dlen x length(which_bmh)
    response <- list( bmh = bmh,
                      qerr = Qerr,
                      mqe = mqe,
                      call = match.call(),
                      method = "suprahex")
        
    invisible(response)
    
}