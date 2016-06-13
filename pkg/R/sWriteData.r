#' Function to write out the best-matching hexagons and/or cluster bases in terms of data
#'
#' \code{sWriteData} is supposed to write out the best-matching hexagons and/or cluster bases in terms of data. 
#'
#' @param sMap an object of class "sMap" or a codebook matrix
#' @param data a data frame or matrix of input data
#' @param sBase an object of class "sBase"
#' @param filename a character string naming a filename
#' @param keep.data logical to indicate whether or not to also write out the input data. By default, it sets to false for not keeping it. It is highly expensive to keep the large data sets
#' @return 
#' a data frame with following components:
#' \itemize{
#'  \item{\code{ID}: ID for data. It inherits the rownames of data (if exists). Otherwise, it is sequential integer values starting with 1 and ending with dlen, the total number of rows of the input data}
#'  \item{\code{Hexagon_index}: the index for best-matching hexagons}
#'  \item{\code{Cluster_base}: optional, it is only appended when sBase is given. It stores the cluster memberships/bases}
#'  \item{\code{data}: optional, it is only appended when keep.data is true}
#' }
#' @note If "filename" is not NULL, a tab-delimited text file will be also written out. If "sBase" is not NULL and comes from the "sMap" partition, then cluster bases are also appended. if "keep.data" is true, the data will be part of output.
#' @export
#' @seealso \code{\link{sBMH}}
#' @include sWriteData.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#'
#' # 2) get trained using by default setup 
#' sMap <- sPipeline(data=data)
#'
#' # 3) write data's BMH hitting the trained map
#' output <- sWriteData(sMap=sMap, data=data, filename="sData_output.txt") 
#'
#' # 4) partition the grid map into cluster bases
#' sBase <- sDmatCluster(sMap=sMap, which_neigh=1,
#' distMeasure="median", clusterLinkage="average") 
#'
#' # 5) write data's BMH and cluster bases
#' output <- sWriteData(sMap=sMap, data=data, sBase=sBase, filename="sData_base_output.txt")

sWriteData <- function(sMap, data, sBase=NULL, filename=NULL, keep.data=F)
{
    
    response <- sBMH(sMap=sMap, data=data, which_bmh="best")    
    
    if(is.null(rownames(data))){
        rownames(data) <- seq(1,nrow(data))
    }
    if(is.null(colnames(data))){
        colnames(data) <- seq(1,ncol(data))
    }
        
    ## Prepare output data
    data_output<- list()
    
    ## 1st column for "ID"
    data_output$ID <- rownames(data)
    
    ## The column for "Hexagon_index"
    bmh <- response$bmh
    colnames(bmh) <- "Hexagon_index"
    data_output$bmh <- bmh
    
    output <- as.data.frame(data_output, stringsAsFactors=F)
    
    ## The column for "Cluster_base" (if sBase is given)
    if(!is.null(sBase)){
        if(class(sBase) == "sBase"){
            if(sMap$nHex == length(sBase$bases)){
                bases <- as.matrix(sBase$bases[bmh])
                colnames(bases) <- "Cluster_base"
                data_output$bases <- bases
                output <- cbind(output, bases)
            }
        }
    }
    
    ## The next columns for data itself (if keep.data is true)
    if(keep.data){
        data_output$data <- data
        output <- cbind(output, data)
    }
    
    ## convert into a data frame called 'output'
    #output <- as.data.frame(data_output, stringsAsFactors=F)
    
    ## If the filename is given, output data is written into a tab-delimited text file
    if(!is.null(filename)){
        write.table(output, file=filename, quote=F, row.names=F, sep="\t")
    }

    invisible(output)
    
}