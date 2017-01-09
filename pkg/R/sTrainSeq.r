#' Function to implement training via sequential algorithm
#'
#' \code{sTrainSeq} is supposed to perform sequential training algorithm. It requires three inputs: a "sMap" or "sInit" object, input data, and a "sTrain" object specifying training environment. The training is implemented iteratively, each training cycle consisting of: i) randomly choose one input vector; ii) determine the winner hexagon/rectangle (BMH) according to minimum distance of codebook matrix to the input vector; ii) update the codebook matrix of the BMH and its neighbors via updating formula (see "Note" below for details). It also returns an object of class "sMap".
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
#'  \item{\code{r}: the hypothetical radius of the grid}
#'  \item{\code{lattice}: the grid lattice}
#'  \item{\code{shape}: the grid shape}
#'  \item{\code{coord}: a matrix of nHex x 2, with each row corresponding to the coordinates of a hexagon/rectangle in the 2D map grid}
#'  \item{\code{init}: an initialisation method}
#'  \item{\code{neighKernel}: the training neighborhood kernel}
#'  \item{\code{codebook}: a codebook matrix of nHex x ncol(data), with each row corresponding to a prototype vector in input high-dimensional space}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note Updating formula is: \eqn{m_i(t+1) = m_i(t) + \alpha(t)*h_{wi}(t)*[x(t)-m_i(t)]}, where 
#' \itemize{
#' \item{\eqn{t} denotes the training time/step}
#' \item{\eqn{i} and \eqn{w} stand for the hexagon/rectangle \eqn{i} and the winner BMH \eqn{w}, respectively}
#' \item{\eqn{x(t)} is an input vector randomly choosen (from the input data) at time \eqn{t}}
#' \item{\eqn{m_i(t)} and \eqn{m_i(t+1)} are respectively the prototype vectors of the hexagon \eqn{i} at time \eqn{t} and \eqn{t+1}}
#' \item{\eqn{\alpha(t)} is the learning rate at time \eqn{t}. There are three types of learning rate functions:}
#' \itemize{
#' \item{For "linear" function, \eqn{\alpha(t)=\alpha_0*(1-t/T)}}
#' \item{For "power" function, \eqn{\alpha(t)=\alpha_0*(0.005/\alpha_0)^{t/T}}}
#' \item{For "invert" function, \eqn{\alpha(t)=\alpha_0/(1+100*t/T)}}
#' \item{Where \eqn{\alpha_0} is the initial learing rate (typically, \eqn{\alpha_0=0.5} at "rough" stage, \eqn{\alpha_0=0.05} at "finetune" stage), \eqn{T} is the length of training time/step (often being set to input data length, i.e., the total number of rows)}
#' }
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
#' @include sTrainSeq.r
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
#' sT_rough <- sTrainology(sMap=sI, data=data, algorithm="sequential", stage="rough") 
#'
#' # 5) training at "rough" stage
#' sM_rough <- sTrainSeq(sMap=sI, data=data, sTrain=sT_rough)
#' 
#' # 6) define trainology at "finetune" stage
#' sT_finetune <- sTrainology(sMap=sI, data=data, algorithm="sequential", stage="finetune")
#'
#' # 7) training at "finetune" stage
#' sM_finetune <- sTrainSeq(sMap=sM_rough, data=data, sTrain=sT_rough)

sTrainSeq <- function(sMap, data, sTrain, verbose=TRUE)
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
    
    ## train length (tlen) is: sTrain$trainLength multiplied by dlen
    tlen <- sTrain$trainLength * dlen
    
    ## neighborhood radius: goes linearly from radiusInitial to radiusFinal
    radiusInitial <- sTrain$radiusInitial
    radiusStep <- (sTrain$radiusFinal - sTrain$radiusInitial)/(tlen - 1)
    
    ## learning rate
    alphaType <- sTrain$alphaType
    alphaInitial <- sTrain$alphaInitial
    if(alphaType == "invert"){
        ## alpha(t) = a / (t+b), where a and b are chosen suitably below, they are chosen so that alphaFinal = alphaInitial/100
        b <- (tlen - 1) / (100 - 1)
        a <- b * alphaInitial
    }
    
    ## neighborhood kernel
    neighKernel <- sTrain$neighKernel
    
    ## distances between hexagons/rectangles in a grid
    Ud <- sHexDist(sObj=sMap)
    Ud <- Ud^2 ## squared Ud (see notes radius below)
    
    ########################################################
    ## A function to indicate the running progress
    progress_indicate <- function(i, B, step, flag=FALSE){
        if(i %% ceiling(B/step) == 0 | i==B | i==1){
            if(flag & verbose){
                message(sprintf("\t%d out of %d (%s)", i, B, as.character(Sys.time())), appendLF=TRUE)
            }
        }
    }
    ##################################################################
    
    updateStep <- min(dlen,1000)
    
    eps <- 1e-16
    set.seed(1234)
    for (t in 1:tlen){
        
        progress_indicate(i=t, B=tlen, 10, flag=TRUE)
        
        ## For every updateStep: re-calculate sample index, neighborhood radius and learning rate
        ind <- t %% updateStep
        if(ind == 0){
            ind <- updateStep
        }
        if(ind == 1){
      
            steps <- t:min(tlen, t+updateStep-1)
            samples <- ceiling(dlen*runif(updateStep) + eps)

            ## neighborhood radius
            radius <- radiusInitial + (steps-1)*radiusStep
            ## squared radius (see notes about Ud above)
            radius <- radius^2
            ## zero radius might cause div-by-zero error 
            radius[radius==0] <- eps
    
            ## learning rate
            if(alphaType == "linear"){
                alpha <- (1-steps/tlen)*alphaInitial
            }else if(alphaType == "invert"){
                alpha <- a/(b + steps-1)
            }else if(alphaType == "power"){
                alpha <- alphaInitial * (0.005/alphaInitial)^((steps-1)/tlen)
            }  
    
        }
        
        ## pick up one sample vector
        x <- matrix(D[samples[ind],], nrow=1, ncol=ncol(D))
        ## codebook matrix minus the sample vector
        Mx <- M - matrix(rep(x, nHex), nrow=nHex, ncol=ncol(M), byrow=TRUE)
        ## Find best-matching hexagon/rectangle (BMH) according to minumum distance of codebook matrix from the sample vector
        tmpDist <- apply(Mx^2,1,sum)
        qerr <- min(tmpDist)
        bmh <- min(which(tmpDist == qerr)) ## the first index (if many)

        ## neighborhood kernel and radius
        ## notice: Ud and radius have been squared
        if(neighKernel == "bubble"){
            h <- (Ud[,bmh] <= radius[ind])
        }else if(neighKernel == "gaussian"){
            h <- exp(-Ud[,bmh]/(2*radius[ind]))
        }else if(neighKernel == "cutgaussian"){
            h <- exp(-Ud[,bmh]/(2*radius[ind])) * (Ud[,bmh] <= radius[ind])
        }else if(neighKernel == "ep"){
            h <- (1-Ud[,bmh]/radius[ind]) * (Ud[,bmh] <= radius[ind])
        }else if(neighKernel == "gamma"){
            h <- 1/gamma(Ud[,bmh]/(4*radius[ind]) +1+1)
        }
    
        ## alpha(t) * h(t)
        hAlpha <- h*alpha[ind]
        
        ## update item
        if(0){
            updataItem <- matrix(0, nrow=nHex, ncol=ncol(M))
            for(i in 1:nHex){
                updataItem[i,] <- hAlpha[i] * Mx[i,]
            }
        }else{
            tmp <- matrix(rep(hAlpha,dim(Mx)[2]), ncol=dim(Mx)[2])
            updataItem <- tmp * Mx
        }
        
        ## update M
        M <- M - updataItem
    }
    ##################################################################
    
    sMap <- list(  nHex = nHex, 
                   xdim = xdim, 
                   ydim = ydim,
                   r = sMap$r,
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