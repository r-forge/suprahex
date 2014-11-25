#' Function to implement training via batch algorithm
#'
#' \code{sTrainBatch} is supposed to perform batch training algorithm. It requires three inputs: a "sMap" or "sInit" object, input data, and a "sTrain" object specifying training environment. The training is implemented iteratively, but instead of choosing a single input vector, the whole input matrix is used. In each training cycle, the whole input matrix first land in the map through identifying the corresponding winner hexagon/rectangle (BMH), and then the codebook matrix is updated via updating formula (see "Note" below for details). It returns an object of class "sMap".
#'
#' @param sMap an object of class "sMap" or "sInit"
#' @param data a data frame or matrix of input data
#' @param sTrain an object of class "sTrain"
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to TRUE for display
#' @return 
#' an object of class "sMap", a list with following components:
#' \itemize{
#'  \item{\code{nHex}: the total number of hexagons/rectanges in the grid}
#'  \item{\code{xdim}: x-dimension of the grid}
#'  \item{\code{ydim}: y-dimension of the grid}
#'  \item{\code{lattice}: the grid lattice}
#'  \item{\code{shape}: the grid shape}
#'  \item{\code{coord}: a matrix of nHex x 2, with each row corresponding to the coordinates of a hexagon/rectangle in the 2D map grid}
#'  \item{\code{init}: an initialisation method}
#'  \item{\code{neighKernel}: the training neighborhood kernel}
#'  \item{\code{codebook}: a codebook matrix of nHex x ncol(data), with each row corresponding to a prototype vector in input high-dimensional space}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note Updating formula is: \eqn{m_i(t+1) = \frac{\sum_{j=1}^{dlen}h_{wi}(t)x_j}{\sum_{j=1}^{dlen}h_{wi}(t)}}, where 
#' \itemize{
#' \item{\eqn{t} denotes the training time/step}
#' \item{\eqn{x_j} is an input vector \eqn{j} from the input data matrix (with \eqn{dlen} rows in total)}
#' \item{\eqn{i} and \eqn{w} stand for the hexagon/rectangle \eqn{i} and the winner BMH \eqn{w}, respectively}
#' \item{\eqn{m_i(t+1)} is the prototype vector of the hexagon \eqn{i} at time \eqn{t+1}}
#' \item{\eqn{h_{wi}(t)} is the neighborhood kernel, a non-increasing function of i) the distance \eqn{d_{wi}} between the hexagon/rectangle \eqn{i} and the winner BMH \eqn{w}, and ii) the radius \eqn{\delta_t} at time \eqn{t}. There are five kernels available:}
#' \itemize{
#' \item{For "gaussian" kernel, \eqn{h_{wi}(t)=e^{-d_{wi}^2/(2*\delta_t^2)}}}
#' \item{For "cutguassian" kernel, \eqn{h_{wi}(t)=e^{-d_{wi}^2/(2*\delta_t^2)}*(d_{wi} \le \delta_t)}}
#' \item{For "bubble" kernel, \eqn{h_{wi}(t)=(d_{wi} \le \delta_t)}}
#' \item{For "ep" kernel, \eqn{h_{wi}(t)=(1-d_{wi}^2/\delta_t^2)*(d_{wi} \le \delta_t)}}
#' \item{For "gamma" kernel, \eqn{h_{wi}(t)=1/\Gamma(d_{wi}^2/(4*\delta_t^2)+2)}}
#' }
#' }
#' @export
#' @seealso \code{\link{sTrainology}}, \code{\link{visKernels}}
#' @include sTrainBatch.r
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

sTrainBatch <- function(sMap, data, sTrain, verbose=T)
{

    if (class(sMap) != "sMap" & class(sMap) != "sInit"){
        stop("The funciton must apply to either 'sMap' or 'sInit' object.\n")
    }
    xdim <- sMap$xdim
    ydim <- sMap$ydim
    nHex <- sMap$nHex
    M <- sMap$codebook
    if (is.vector(M)){
        M <- matrix(M, nrow=1, ncol=length(M))
    }

    if (is.vector(data)){
        data <- matrix(data, nrow=1, ncol=length(data))
    }else if(is.matrix(data) | is.data.frame(data)){
        data <- as.matrix(data)
    }else if(is.null(data)){
        stop("The input data must be not NULL.\n")
    }
    dlen <- nrow(data)
    D <- data
    
    if (class(sTrain) != "sTrain" ){
        stop("The funciton must apply to a 'sTrain' object.\n")
    }
    
    ########### Extracting trainology ###########
    
    ## train length (tlen) is: sTrain$trainLength (unlike in 'sTrainSeq', this should NOT be multiplied by dlen)
    tlen <- sTrain$trainLength
    
    ## neighborhood radius: goes linearly from radiusInitial to radiusFinal
    radiusInitial <- sTrain$radiusInitial
    radiusFinal <- sTrain$radiusFinal
    if(tlen == 1){
        radius <- radiusInitial
    }else{
        radius <- radiusFinal + ((tlen-1):0)/((tlen-1)) * (radiusInitial - radiusFinal)
    }

    radius <- radius^2;
    ## avoid div-by-zero error
    eps <- 1e-16
    radius[radius==0] <- eps

    ## neighborhood kernel
    neighKernel <- sTrain$neighKernel
    
    ## distances between hexagons/rectangles in a grid
    Ud <- sHexDist(sObj=sMap)
    Ud <- Ud^2 ## squared Ud (see notes radius below)
    
    ########################################################
    ## A function to indicate the running progress
    progress_indicate <- function(i, B, step, flag=F){
        if(i %% ceiling(B/step) == 0 | i==B | i==1){
            if(flag & verbose){
                message(sprintf("\t%d out of %d (%s)", i, B, as.character(Sys.time())), appendLF=T)
            }
        }
    }
    ##################################################################
    
    for (t in 1:length(radius)){
        
        progress_indicate(i=t, B=length(radius), length(radius), flag=T)
        
        if(1){
            response <- sBMH(M, D, which_bmh="best")
            # bmh: the requested BMH matrix of dlen x length(which_bmh)
            # qerr: the corresponding matrix of quantization errors
            # mqe: average quantization error
            bmh <- response$bmh
        }else{
            bmh <- matrix(0, nrow=dlen, ncol=1)
            for (i in 1:dlen){
                tmp_dist <- matrix(0, nrow=nHex, ncol=1)
                for (j in 1:nHex){
                    tmp_dist[j] <- sum((D[i,] - M[j,])^2)
                }
                bmh[i] <- min(which(tmp_dist == min(tmp_dist)))
            }
        }
        
        ## neighborhood kernel and radius
        ## notice: Ud and radius have been squared
        if(neighKernel == "bubble"){
            H <- (Ud <= radius[t])
        }else if(neighKernel == "gaussian"){
            H <- exp(-Ud/(2*radius[t]))
        }else if(neighKernel == "cutgaussian"){
            H <- exp(-Ud/(2*radius[t])) * (Ud <= radius[t])
        }else if(neighKernel == "ep"){
            H <- (1-Ud/radius[t]) * (Ud <= radius[t])
        }else if(neighKernel == "gamma"){
            H <- 1/gamma(Ud/(4*radius[t]) +1+1)
        }
        
        Hi <- H[,bmh]
        S <- Hi %*% D
        #A <- Hi %*% (D != 0)
        A <- Hi %*% !is.na(D)
        
        if(verbose){
            message(sprintf("\tupdated (%s)", as.character(Sys.time())), appendLF=T)
        }
        
        # only update units for which the "activation" is nonzero
        nonzero <- which(A > 0) 
        M[nonzero] <- S[nonzero] / A[nonzero] 
        
    }
    ##################################################################
    
    sMap <- list(  nHex = nHex, 
                   xdim = xdim, 
                   ydim = ydim,
                   lattice = sMap$lattice,
                   shape = sMap$shape,
                   coord = sMap$coord,
                   init = sMap$init,
                   neighKernel = sTrain$neighKernel,
                   codebook = M,
                   call = match.call(),
                   method = "suprahex")
    
    class(sMap) <- "sMap"
    
    invisible(sMap)
}