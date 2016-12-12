#' Function to define trainology (training environment)
#'
#' \code{sTrainology} is supposed to define the train-ology (i.e., the training environment/parameters). The trainology here refers to the training algorithm, the training stage, the stage-specific parameters (alpha type, initial alpha, initial radius, final radius and train length), and the training neighbor kernel used. It returns an object of class "sTrain".
#'
#' @param sMap an object of class "sMap" or "sInit"
#' @param data a data frame or matrix of input data
#' @param algorithm the training algorithm. It can be one of "sequential" and "batch" algorithm
#' @param stage the training stage. The training can be achieved using two stages (i.e., "rough" and "finetune") or one stage only (i.e., "complete")
#' @param alphaType the alpha type. It can be one of "invert", "linear" and "power" alpha types
#' @param neighKernel the training neighbor kernel. It can be one of "gaussian", "bubble", "cutgaussian", "ep" and "gamma" kernels
#' @return 
#' an object of class "sTrain", a list with following components:
#' \itemize{
#'  \item{\code{algorithm}: the training algorithm}
#'  \item{\code{stage}: the training stage}
#'  \item{\code{alphaType}: the alpha type}
#'  \item{\code{alphaInitial}: the initial alpha}
#'  \item{\code{radiusInitial}: the initial radius}
#'  \item{\code{radiusFinal}: the final radius}
#'  \item{\code{neighKernel}: the neighbor kernel}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note Training stage-specific parameters: 
#' \itemize{
#' \item{"radiusInitial": it depends on the grid shape and training stage}
#' \itemize{
#' \item{For "sheet" shape: it equals \eqn{max(1,ceiling(max(xdim,ydim)/8))} at "rough" or "complete" stage, and \eqn{max(1,ceiling(max(xdim,ydim)/32))} at "finetune" stage}
#' \item{For "suprahex" shape: it equals \eqn{max(1,ceiling(r/2))} at "rough" or "complete" stage, and \eqn{max(1,ceiling(r/8))} at "finetune" stage}
#' }
#' \item{"radiusFinal": it depends on the training stage}
#' \itemize{
#' \item{At "rough" stage, it equals \eqn{radiusInitial/4}}
#' \item{At "finetune" or "complete" stage, it equals \eqn{1}}
#' }
#' \item{"trainLength": how many times the whole input data are set for training. It depends on the training stage and training algorithm}
#' \itemize{
#' \item{At "rough" stage, it equals \eqn{max(1,10 * trainDepth)}}
#' \item{At "finetune" stage, it equals \eqn{max(1,40 * trainDepth)}}
#' \item{At "complete" stage, it equals \eqn{max(1,50 * trainDepth)}}
#' \item{When using "batch" algorithm and the trainLength equals 1 according to the above equation, the trainLength is forced to be 2 unless \eqn{radiusInitial} equals \eqn{radiusFinal}}
#' \item{Where \eqn{trainDepth} is the training depth, defined as \eqn{nHex/dlen}, i.e., how many hexagons/rectanges are used per the input data length (here \eqn{dlen} refers to the number of rows)}
#' }
#' }
#' @export
#' @seealso \code{\link{sInitial}}
#' @include sTrainology.r
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
#' # 4) define trainology at different stages
#' # 4a) define trainology at "rough" stage
#' sT_rough <- sTrainology(sMap=sI, data=data, stage="rough") 
#' # 4b) define trainology at "finetune" stage
#' sT_finetune <- sTrainology(sMap=sI, data=data, stage="finetune") 
#' # 4c) define trainology using "complete" stage
#' sT_complete <- sTrainology(sMap=sI, data=data, stage="complete")

sTrainology <- function(sMap, data, algorithm=c("batch","sequential"), stage=c("rough","finetune","complete"), alphaType=c("invert","linear","power"), neighKernel=c("gaussian","bubble","cutgaussian","ep","gamma"))
{
    
    algorithm <- match.arg(algorithm)
    stage <- match.arg(stage)
    alphaType <- match.arg(alphaType)
    neighKernel <- match.arg(neighKernel)
        
    if (class(sMap) != "sMap" & class(sMap) != "sInit"){
        stop("The funciton must apply to either 'sMap' or 'sInit' object.\n")
    }
    
    if (is.vector(data)){
        data <- matrix(data, nrow=1, ncol=length(data))
    }else if(is.matrix(data) | is.data.frame(data)){
        data <- as.matrix(data)
    }else if(is.null(data)){
        stop("The input data must be not NULL.\n")
    }
    
    dlen <- nrow(data) ## dlen is the number of rows of input data
    xdim <- sMap$xdim
    ydim <- sMap$ydim
    shape <- sMap$shape
    nHex <- sMap$nHex
    
    ## training depth being defined as nHex per dlen
    trainDepth <- nHex/dlen
    
    ## By default the learning rate (alpha) goes from the alphaInitial to 0 along the function defined by alphaType
    ## By default the neighborhood radius goes linearly from radiusInitial to radiusFinal
    
    ## At rough stage, the trainLength is set to 10 x trainDepth
    ## At finetune stage, the trainLength is set to 40 x trainDepth
    
    if(algorithm == "sequential"){
        if(stage == "rough"){

            alphaInitial <- 0.5
            
            if(shape == "sheet"){
                radiusInitial <- max(1,ceiling(max(xdim,ydim)/8))
                radiusFinal <- max(1,radiusInitial/4)
            }else if(shape != "sheet"){
                r <- (xdim+1)/2
                radiusInitial <- max(1,ceiling(r/2))
                radiusFinal <- max(1,radiusInitial/4)
            }
            
            trainLength <- max(1,ceiling(10*trainDepth))
            
        }else if(stage=="finetune"){
            
            alphaInitial <- 0.05
            
            if(shape == "sheet"){
                radiusInitial <- max(1,ceiling(max(xdim,ydim)/32)) # i.e., radiusFinal at rough stage
                radiusFinal <- 1
            }else if(shape != "sheet"){
                r <- (xdim+1)/2
                radiusInitial <- max(1,ceiling(r/2))
                radiusInitial <- max(1,radiusInitial/4) # i.e., radiusFinal at rough stage
                radiusFinal <- 1
            }
            
            trainLength <- max(1,ceiling(40*trainDepth))
            
        }else if(stage=="complete"){
            
            alphaInitial <- 0.5
            
            if(shape == "sheet"){
                radiusInitial <- max(1,ceiling(max(xdim,ydim)/8))
                radiusFinal <- 1
            }else if(shape != "sheet"){
                r <- (xdim+1)/2
                radiusInitial <- max(1,ceiling(r/2))
                radiusFinal <- 1
            }
            
            trainLength=max(1,ceiling(50*trainDepth))
            
        }
    }else if(algorithm == "batch"){
        if(stage == "rough"){

            alphaInitial <- NA
            
            if(shape == "sheet"){
                radiusInitial <- max(1,ceiling(max(xdim,ydim)/8))
                radiusFinal <- max(1,radiusInitial/4)
            }else if(shape != "sheet"){
                r <- (xdim+1)/2
                radiusInitial <- max(1,ceiling(r/2))
                radiusFinal <- max(1,radiusInitial/4)
            }
            
            trainLength <- max(1,ceiling(10*trainDepth))
            
        }else if(stage=="finetune"){
            
            alphaInitial <- NA
            
            if(shape == "sheet"){
                radiusInitial <- max(1,ceiling(max(xdim,ydim)/32)) # i.e., radiusFinal at rough stage
                radiusFinal <- 1
            }else if(shape != "sheet"){
                r <- (xdim+1)/2
                radiusInitial <- max(1,ceiling(r/2))
                radiusInitial <- max(1,radiusInitial/4) # i.e., radiusFinal at rough stage
                radiusFinal <- 1
            }
            
            trainLength <- max(1,ceiling(40*trainDepth))
            
        }else if(stage=="complete"){
            
            alphaInitial <- NA
            
            if(shape == "sheet"){
                radiusInitial <- max(1,ceiling(max(xdim,ydim)/8))
                radiusFinal <- 1
            }else if(shape != "sheet"){
                r <- (xdim+1)/2
                radiusInitial <- max(1,ceiling(r/2))
                radiusFinal <- 1
            }
            
            trainLength=max(1,ceiling(50*trainDepth))
            
        }
        
        ## trainLength is always no less than 2 if radiusInitial is larger than radiusFinal
        if(trainLength == 1){
            if(radiusInitial > radiusFinal){
                trainLength <- 2
            }
        }
        
    }
    
    sTrain <- list(algorithm = algorithm,
                   stage = stage,
                   alphaType = alphaType,
                   alphaInitial = alphaInitial, 
                   radiusInitial = radiusInitial, 
                   radiusFinal = radiusFinal,
                   trainLength = trainLength,
                   neighKernel = neighKernel,
                   call = match.call(),
                   method = "suprahex")
    
    class(sTrain) <- "sTrain"
    
    invisible(sTrain)
}