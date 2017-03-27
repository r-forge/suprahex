#' Function to visualise gene clusters/bases partitioned from a supra-hexagonal grid using heatmap
#'
#' \code{visDmatHeatmap} is supposed to visualise gene clusters/bases partitioned from a supra-hexagonal grid using heatmap
#'
#' @param sMap an object of class "sMap" or a codebook matrix
#' @param data a data frame or matrix of input data
#' @param sBase an object of class "sBase"
#' @param base.color short name for the colormap used to encode bases (in row side bar). It can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), and "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta). Alternatively, any hyphen-separated HTML color names, e.g. "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}
#' @param base.separated.arg a list of main parameters used for styling bar separated lines. See 'Note' below for details on the parameters
#' @param base.legend.location location of legend to describe bases. If "none", this legend will not be displayed
#' @param reorderRow the way to reorder the rows within a base. It can be "none" for rows within a base being reorded by the hexagon indexes, "hclust" for rows within a base being reorded according to hierarchical clustering of patterns seen, "svd" for rows within a base being reorded according to svd of patterns seen 
#' @param keep.data logical to indicate whether or not to also write out the input data. By default, it sets to false for not keeping it. It is highly expensive to keep the large data sets
#' @param ... additional graphic parameters used in "visHeatmapAdv". For most parameters, please refer to \url{https://www.rdocumentation.org/packages/gplots/topics/heatmap.2}
#' @return 
#' a data frame with following components:
#' \itemize{
#'  \item{\code{ID}: ID for data. It inherits the rownames of data (if exists). Otherwise, it is sequential integer values starting with 1 and ending with dlen, the total number of rows of the input data}
#'  \item{\code{Hexagon_index}: the index for best-matching hexagons}
#'  \item{\code{Cluster_base}: optional, it is only appended when sBase is given. It stores the cluster memberships/bases}
#'  \item{\code{data}: optional, it is only appended when keep.data is true}
#' }
#' Note: the returned data has rows in the same order as visualised in the heatmap
#' @note A list of parameters in "base.separated.arg":
#' \itemize{
#' \item{"lty": the line type. Line types can either be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as one of the character strings "blank","solid","dashed","dotted","dotdash","longdash","twodash", where "blank" uses 'invisible lines' (i.e., does not draw them)}
#' \item{"lwd": the line width}
#' \item{"col": the line color}
#' }
#' @export
#' @seealso \code{\link{sDmatCluster}}, \code{\link{visHeatmapAdv}}
#' @include visDmatHeatmap.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#'
#' # 2) get trained using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) partition the grid map into clusters using region-growing algorithm
#' sBase <- sDmatCluster(sMap=sMap, which_neigh=1, 
#' distMeasure="median", clusterLinkage="average")
#' 
#' # 4) heatmap visualisation
#' output <- visDmatHeatmap(sMap, data, sBase, base.legend.location="bottomleft", labRow=NA)

visDmatHeatmap <- function (sMap, data, sBase, base.color="rainbow", base.separated.arg=NULL, base.legend.location=c("none","bottomleft","bottomright","bottom","left","topleft","top","topright","right","center"), reorderRow=c("none","hclust","svd"), keep.data=FALSE, ...)
{
    
    base.legend.location <- match.arg(base.legend.location)
    reorderRow <- match.arg(reorderRow)
    
    output <- sWriteData(sMap, data, sBase, filename=NULL, keep.data=keep.data)
    hexagon <- output[,2]
    base <- output[,3]
    
    ###########################################################
    ## contruct data frame for ordering
    if(reorderRow=="none"){
        ## contruct data frame including 1st column for temporary index, 2nd for hexagon index, 3rd for base/cluster ID
        df <- data.frame(ind=1:nrow(output), hexagon, base)
        # order by: first base, then hexagon
        ordering <- df[order(base,hexagon),]$ind
    }else if(reorderRow=="hclust"){
        ## reordering via hierarchical clustering
        cluster_order <- matrix(1, nrow=length(base))
        for(i in 1:length(unique(base))){
            tmpD <- data[base==i,]
            if(sum(base==i) != 1){
                distance <- as.dist(sDistance(tmpD, metric="euclidean"))
                cluster <- hclust(distance, method="complete")
                cluster_order[base==i] <- cluster$order
            }
        }
        ## contruct data frame including 1st column for temporary index, 2nd for cluster order, 3rd for base/cluster ID
        df <- data.frame(ind=1:nrow(output), cluster_order, base)
        # order by: first base, then hexagon
        ordering <- df[order(base,cluster_order),]$ind
    }else if(reorderRow=="svd"){
        ## reordering via SVD
        data <- as.matrix(data)
        svd_order <- matrix(1, nrow=length(base))
        for(i in 1:length(unique(base))){
            tmpD <- data[base==i,]
            if(sum(base==i) != 1){
                sorted <- sort.int(tmpD %*% svd(data)$v[,1], decreasing=TRUE, index.return=TRUE)
                svd_order[base==i] <- sorted$ix
            }
        }
        ## contruct data frame including 1st column for temporary index, 2nd for svd order, 3rd for base/cluster ID
        df <- data.frame(ind=1:nrow(output), svd_order, base)
        # order by: first base, then hexagon
        ordering <- df[order(base,svd_order),]$ind
    }
    
    ######################################################################################
    ## The genes are ordered according to the base/cluster memberships
    D <- data[ordering, ]
    bases <- base[ordering]
    # prepare colors for the row sidebar of heatmap
    # color for bases/clusters
    lvs <- unique(bases)
    lvs_color <- visColormap(colormap=base.color)(length(lvs))
    lvs_color <- visColoralpha(lvs_color, alpha=0.8) # add transparent (alpha) into colors
    col_bases <- sapply(bases, function(x) lvs_color[x==lvs])
    RowSideColors <- matrix(col_bases, nrow=1)
    rownames(RowSideColors) <- paste("Bases ",1,"-",length(unique(bases)), sep="") 
    
    ################################################
    ## add separated lines between bases
    
    ## a function to update the parameters
    update_parameters <- function(default, update){
        for(i in 1:length(names(default))){
            item <- names(default)[i]
            if(item %in% names(update)){
                default[i] <- update[which(names(update)==item)]
            }
        }
        return(default)
    }

    #base.separated.arg <- list(col="black")
    ## the default parameters for "base.separated"
    base.separated.arg.default <- list(
        lty = 5,
        lwd = 1,
        col = "black"
        )
    ## update parameters for "base.separated"
    base.separated.arg.default <- update_parameters(base.separated.arg.default, base.separated.arg)
    
    basesep_index <- sapply(unique(bases), function(x) which(bases[length(bases):1]==x)[1])
    basesep_index <- basesep_index[1:length(basesep_index)-1]
    ################################################
    
    ## heatmap embeded with sidebars annotating gene cluster memberships
    visHeatmapAdv(D, Rowv=FALSE, Colv=FALSE, RowSideColors=RowSideColors, add.expr=abline(h=(basesep_index-0.5), lty=base.separated.arg.default$lty,lwd=base.separated.arg.default$lwd,col=base.separated.arg.default$col), ...)
    
    ## add legend
    if(base.legend.location!="none"){
        legend_txt <- paste(rep("Base",length(lvs)), lvs, sep=" ")
        legend(base.legend.location, legend=legend_txt, col=lvs_color, lty=1, lwd=5, cex=0.6, box.col="transparent", horiz=FALSE)
    }
    
    ######################################################################################
    invisible(output[ordering,])
}