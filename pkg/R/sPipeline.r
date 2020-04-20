#' Function to setup the pipeline for completing ab initio training given the input data
#'
#' \code{sPipeline} is supposed to finish ab inito training for the input data. It returns an object of class "sMap". 
#'
#' @param data a data frame or matrix of input data
#' @param xdim an integer specifying x-dimension of the grid
#' @param ydim an integer specifying y-dimension of the grid
#' @param nHex the number of hexagons/rectangles in the grid
#' @param lattice the grid lattice, either "hexa" for a hexagon or "rect" for a rectangle
#' @param shape the grid shape, either "suprahex" for a supra-hexagonal grid or "sheet" for a hexagonal/rectangle sheet. Also supported are suprahex's variants (including "triangle" for the triangle-shaped variant, "diamond" for the diamond-shaped variant, "hourglass" for the hourglass-shaped variant, "trefoil" for the trefoil-shaped variant, "ladder" for the ladder-shaped variant, "butterfly" for the butterfly-shaped variant, "ring" for the ring-shaped variant, and "bridge" for the bridge-shaped variant)
#' @param scale the scaling factor. Only used when automatically estimating the grid dimension from input data matrix. By default, it is 5 (big map). Other suggested values: 1 for small map, and 3 for median map 
#' @param init an initialisation method. It can be one of "uniform", "sample" and "linear" initialisation methods
#' @param algorithm the training algorithm. It can be one of "sequential" and "batch" algorithm. By default, it uses 'batch' algorithm purely because of its fast computations (probably also without the compromise of accuracy). However, it is highly recommended not to use 'batch' algorithm if the input data contain lots of zeros; it is because matrix multiplication used in the 'batch' algorithm can be problematic in this context. If much computation resource is at hand, it is alwasy safe to use the 'sequential' algorithm  
#' @param alphaType the alpha type. It can be one of "invert", "linear" and "power" alpha types
#' @param neighKernel the training neighborhood kernel. It can be one of "gaussian", "bubble", "cutgaussian", "ep" and "gamma" kernels
#' @param finetuneSustain logical to indicate whether sustain the "finetune" training. If true, it will repeat the "finetune" stage until the mean quantization error does get worse. By default, it sets to true
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to false for no display
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
#'  \item{\code{polygon}: a data frame of three columns ('x','y','id') storing polygon location per hexagon in the 2D map grid}
#'  \item{\code{init}: an initialisation method}
#'  \item{\code{neighKernel}: the training neighborhood kernel}
#'  \item{\code{codebook}: a codebook matrix of nHex x ncol(data), with rows corresponding to prototype vectors in input high-dimensional space}
#'  \item{\code{hits}: a vector of nHex, each element meaning that a hexagon/rectangle contains the number of input data vectors being hit wherein}
#'  \item{\code{mqe}: the mean quantization error for the "best" BMH}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note The pipeline sequentially consists of: 
#' \itemize{
#' \item{i) \code{\link{sTopology}} used to define the topology of a grid (with "suprahex" shape by default ) according to the input data;}
#' \item{ii) \code{\link{sInitial}} used to initialise the codebook matrix given the pre-defined topology and the input data (by default using "uniform" initialisation method);}
#' \item{iii) \code{\link{sTrainology}} and \code{\link{sTrainSeq}} used to get the grid map trained at both "rough" and "finetune" stages. If instructed, sustain the "finetune" training until the mean quantization error does get worse;}
#' \item{iv) \code{\link{sBMH}} used to identify the best-matching hexagons/rectangles (BMH) for the input data, and these response data are appended to the resulting object of "sMap" class.}
#' }
#' @export
#' @import hexbin
#' @import MASS
#' @importFrom ape nj fastme.ols fastme.bal boot.phylo consensus is.rooted unroot root mrca write.tree read.tree plot.phylo nodelabels
#' @importFrom grDevices col2rgb colorRampPalette dev.new hsv rainbow rgb2hsv
#' @importFrom graphics abline axis box hist image layout legend lines mtext par plot.new points rect stars strheight strwidth symbols text title
#' @importFrom stats as.dendrogram as.dist cor cutree density dist hclust heatmap median order.dendrogram quantile reorder runif sd
#' @importFrom utils write.table
#' @importFrom methods is
#' @seealso \code{\link{sTopology}}, \code{\link{sInitial}}, \code{\link{sTrainology}}, \code{\link{sTrainSeq}}, \code{\link{sTrainBatch}}, \code{\link{sBMH}}, \code{\link{visHexMulComp}}
#' @include sPipeline.r
#' @references
#' Hai Fang and Julian Gough. (2014) supraHex: an R/Bioconductor package for tabular omics data analysis using a supra-hexagonal map. \emph{Biochemical and Biophysical Research Communications}, 443(1), 285-289.
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#' colnames(data) <- paste(rep('S',10), seq(1:10), sep="")
#'
#' # 2) get trained using by default setup but with different neighborhood kernels
#' # 2a) with "gaussian" kernel
#' sMap <- sPipeline(data=data, neighKernel="gaussian")
#' # 2b) with "bubble" kernel
#' # sMap <- sPipeline(data=data, neighKernel="bubble")
#' # 2c) with "cutgaussian" kernel
#' # sMap <- sPipeline(data=data, neighKernel="cutgaussian")
#' # 2d) with "ep" kernel
#' # sMap <- sPipeline(data=data, neighKernel="ep")
#' # 2e) with "gamma" kernel
#' # sMap <- sPipeline(data=data, neighKernel="gamma")
#' 
#' # 3) visualise multiple component planes of a supra-hexagonal grid
#' visHexMulComp(sMap, colormap="jet", ncolors=20, zlim=c(-1,1), gp=grid::gpar(cex=0.8))
#' 
#' # 4) get trained using by default setup but using the shape "butterfly"
#' sMap <- sPipeline(data=data, shape="trefoil", algorithm=c("batch","sequential")[2])
#' visHexMulComp(sMap, colormap="jet", ncolors=20, zlim=c(-1,1), gp=grid::gpar(cex=0.8))
#' 
#' }

sPipeline <- function(data=NULL, xdim=NULL, ydim=NULL, nHex=NULL, lattice=c("hexa","rect"), shape=c("suprahex", "sheet", "triangle", "diamond", "hourglass", "trefoil", "ladder", "butterfly", "ring", "bridge"), scale=5, init=c("linear","uniform","sample"), algorithm=c("batch","sequential"), alphaType=c("invert","linear","power"), neighKernel=c("gaussian","bubble","cutgaussian","ep","gamma"), finetuneSustain=FALSE, verbose=TRUE)
{

    startT <- Sys.time()
    message(paste(c("Start at ",as.character(startT)), collapse=""), appendLF=TRUE)
    message("", appendLF=TRUE)
    ####################################################################################
    
    ## match.arg matches arg against a table of candidate values as specified by choices, where NULL means to take the first one
    lattice <- match.arg(lattice)
    shape <- match.arg(shape)
    init <- match.arg(init)
    algorithm <- match.arg(algorithm)
    alphaType <- match.arg(alphaType)
    neighKernel <- match.arg(neighKernel)
    
    ## define the topology of a map grid
    if (verbose){
        now <- Sys.time()
        message(sprintf("First, define topology of a map grid (%s)...", as.character(now)), appendLF=TRUE)
    }
    sTopol <- sTopology(data=data, xdim=xdim, ydim=ydim, nHex=nHex, lattice=lattice, shape=shape, scale=scale)
    
    ## initialise the codebook matrix given a topology and input data
    if (verbose){
        message(sprintf("Second, initialise the codebook matrix (%d X %d) using '%s' initialisation, given a topology and input data (%s)...", sTopol$nHex, ncol(data), init, as.character(now)), appendLF=TRUE)
    }
    sI <- sInitial(data=data, sTopol=sTopol, init=init)
    
    ## get training at the rough stage
    if (verbose){
        now <- Sys.time()
        message(sprintf("Third, get training at the rough stage (%s)...", as.character(now)), appendLF=TRUE)
    }
    sT_rough <- sTrainology(sMap=sI, data=data, algorithm=algorithm, stage="rough", alphaType=alphaType, neighKernel=neighKernel)
    if(algorithm == "sequential"){
        sM_rough <- sTrainSeq(sMap=sI, data=data, sTrain=sT_rough)
    }else{
        sM_rough <- sTrainBatch(sMap=sI, data=data, sTrain=sT_rough)
    }

    ## get training at the finetune stage
    if (verbose){
        now <- Sys.time()
        message(sprintf("Fourth, get training at the finetune stage (%s)...", as.character(now)), appendLF=TRUE)
    }
    sT_finetune <- sTrainology(sMap=sI, data=data, algorithm=algorithm, stage="finetune", alphaType=alphaType, neighKernel=neighKernel)
    if(algorithm == "sequential"){
        sM_finetune <- sTrainSeq(sMap=sM_rough, data=data, sTrain=sT_finetune)
    }else{
        sM_finetune <- sTrainBatch(sMap=sM_rough, data=data, sTrain=sT_finetune)
    }
    
    if(finetuneSustain){
        ## identify the best-matching hexagon/rectangle for the input data
        ##cat("Identify the best-matching hexagon/rectangle for the input data...\n",append=FALSE)
        response <- sBMH(sMap=sM_finetune, data=data, which_bmh="best")
        # bmh: the requested BMH matrix of dlen x length(which_bmh)
        # qerr: the corresponding matrix of quantization errors
        # mqe: average quantization error
    
        ## sustain the finetune training till the mean quantization error (mqe) does not get worsen
        if (verbose){
            now <- Sys.time()
            message(sprintf("Fifth, sustain the next 10 rounds of finetune training till the mean quantization error (mqe) does get worse (%s)...", as.character(now)), appendLF=TRUE)
        }
        mqe <- vector()
        k=1
        mqe[k] <- round(response$mqe * 10)/10
    
        if(verbose){
            message <- paste(c("\t", k, " iteration ", "with current mqe=", mqe[k]), collapse="")
            message(message, appendLF=TRUE)
        }
    
        sM_now <- sM_finetune
        flag <- 1
        while(flag){
            sM_pre <- sM_now
        
            if(algorithm == "sequential"){
                sM_now <- sTrainSeq(sMap=sM_pre, data=data, sTrain=sT_finetune)
            }else{
                sM_now <- sTrainBatch(sMap=sM_pre, data=data, sTrain=sT_finetune)
            }
            response <- sBMH(sMap=sM_now, data=data, which_bmh="best")

            k <- k+1
            mqe[k] <- round(response$mqe * 10)/10
            if((mqe[k] >= mqe[k-1]) | k == 10){
                flag <- 0
            }
        
            if(verbose){
                message <- paste(c("\t", k, " iteration ", "with current mqe=", mqe[k]), collapse="")
                message(message, appendLF=TRUE)
            }
        }
        
        sM_final <- sM_pre
    }else{
        sM_final <- sM_finetune
    }
    
    if (verbose){
        now <- Sys.time()
        message(sprintf("Next, identify the best-matching hexagon/rectangle for the input data (%s)...", as.character(now)), appendLF=TRUE)
    }
    response <- sBMH(sMap=sM_final, data=data, which_bmh="best")
    
    ##################################################################
    if (verbose){
        now <- Sys.time()
        message(sprintf("Finally, append the response data (hits and mqe) into the sMap object (%s)...", as.character(now)), appendLF=TRUE)
    }
    
    ## for hits
    hits <- sapply(seq(1,sM_final$nHex), function(x) sum(response$bmh==x))
    
    ##################
    ## for df_polygons
    df_polygon <- sHexPolygon(sM_final, area.size=1)
    ##################
        
    sMap <- list(  nHex = sM_final$nHex, 
                   xdim = sM_final$xdim, 
                   ydim = sM_final$ydim,
                   r = sM_final$r,
                   lattice = sM_final$lattice,
                   shape = sM_final$shape,
                   coord = sM_final$coord,
                   polygon = df_polygon,
                   init = sM_final$init,
                   neighKernel = sM_final$neighKernel,
                   codebook = sM_final$codebook,
                   hits = hits,
                   mqe = response$mqe,
                   call = match.call(),
                   method = "suprahex")
                   
    class(sMap) <- "sMap"
    
    if(verbose){
    
        message("", appendLF=TRUE)
    
        message("Below are the summaries of the training results:", appendLF=TRUE)
        summary <- vector()
        summary[1] <- paste(c("   dimension of input data: ", dim(data)[1], "x", dim(data)[2], "\n"), collapse="")
        summary[2] <- paste(c("   xy-dimension of map grid: ", "xdim=", sMap$xdim, ", ydim=", sMap$ydim,", r=", sMap$r, "\n"), collapse="")
        summary[3] <- paste(c("   grid lattice: ", sMap$lattice, "\n"), collapse="")
        summary[4] <- paste(c("   grid shape: ", sMap$shape, "\n"), collapse="")
        summary[5] <- paste(c("   dimension of grid coord: ", dim(sMap$coord)[1], "x", dim(sMap$coord)[2], "\n"), collapse="")
        summary[6] <- paste(c("   initialisation method: ", sMap$init, "\n"), collapse="")
        summary[7] <- paste(c("   dimension of codebook matrix: ", dim(sMap$codebook)[1], "x", dim(sMap$codebook)[2], "\n"), collapse="")
        summary[8] <- paste(c("   mean quantization error: ", sMap$mqe, "\n"), collapse="")
        message(summary,appendLF=TRUE)
        
        message("Below are the details of trainology:", appendLF=TRUE)
        details <- vector()
        details[1] <- paste(c("   training algorithm: ", algorithm, "\n"), collapse="")
        details[2] <- paste(c("   alpha type: ", alphaType, "\n"), collapse="")
        details[3] <- paste(c("   training neighborhood kernel: ", neighKernel, "\n"), collapse="")
        details[4] <- paste(c("   trainlength (x input data length): ", sT_rough$trainLength," at rough stage; ", sT_finetune$trainLength," at finetune stage", "\n"), collapse="")
        details[5] <- paste(c("   radius (at rough stage): from ", sT_rough$radiusInitial," to ", sT_rough$radiusFinal, "\n"), collapse="")
        details[6] <- paste(c("   radius (at finetune stage): from ", sT_finetune$radiusInitial," to ", sT_finetune$radiusFinal, "\n"), collapse="")
        message(details,appendLF=TRUE)
    }   
    
    ####################################################################################
    endT <- Sys.time()
    message(paste(c("End at ",as.character(endT)), collapse=""), appendLF=TRUE)
    
    runTime <- as.numeric(difftime(strptime(endT, "%Y-%m-%d %H:%M:%S"), strptime(startT, "%Y-%m-%d %H:%M:%S"), units="secs"))
    message(paste(c("Runtime in total is: ",runTime," secs\n"), collapse=""), appendLF=TRUE)
    
    invisible(sMap)
}