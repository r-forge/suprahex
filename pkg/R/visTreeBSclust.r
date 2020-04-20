#' Function to obtain clusters from a bootstrapped tree
#'
#' \code{visTreeBSclust} is supposed to obtain clusters from a bootstrapped tree. 
#'
#' @param tree_bs an "phylo" object storing a bootstrapped tree
#' @param bootstrap.cutoff an integer specifying bootstrap-derived clusters
#' @param max.fraction the maximum fraction of leaves contained in a cluster
#' @param min.size the minumum number of leaves contained in a cluster 
#' @param visTree logical to indicate whether the tree will be visualised. By default, it sets to true for display
#' @param plot.phylo.arg a list of main parameters used in the function "ape::plot.phylo" \url{http://rdrr.io/cran/ape/man/plot.phylo.html}. See 'Note' below for details on the parameters
#' @param nodelabels.arg a list of main parameters used in the function "ape::nodelabels" \url{http://rdrr.io/cran/ape/man/nodelabels.html}. See 'Note' below for details on the parameters
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to true for display
#' @param ... additional "ape::plot.phylo" parameters
#' @return 
#' a data frame following components:
#' \itemize{
#'  \item{\code{Samples}: the labels for tip nodes (samples)}
#'  \item{\code{Clusters}: the clusters each tip node belongs to; unassigned tip nodes will be the cluster called 'C0'}
#'  \item{\code{Clans}: the internal node id for each cluster}
#' }
#' @note 
#' A list of main parameters used in the function "ape::plot.phylo":
#' \itemize{
#' \item{"type": a character string specifying the type of phylogeny to be drawn; it must be one of "phylogram" (the default), "cladogram", "fan", "unrooted", "radial" or any unambiguous abbreviation of these}
#' \item{"direction": a character string specifying the direction of the tree. Four values are possible: "rightwards" (the default), "leftwards", "upwards", and "downwards"}
#' \item{"lab4ut": (= labels for unrooted trees) a character string specifying the display of tip labels for unrooted trees: either "horizontal" where all labels are horizontal (the default), or "axial" where the labels are displayed in the axis of the corresponding terminal branches. This option has an effect only if type = "unrooted"}
#' \item{"edge.color": a vector of mode character giving the colours used to draw the branches of the plotted phylogeny. These are taken to be in the same order than the component edge of phy. If fewer colours are given than the length of edge, then the colours are recycled}
#' \item{"edge.width": a numeric vector giving the width of the branches of the plotted phylogeny. These are taken to be in the same order than the component edge of phy. If fewer widths are given than the length of edge, then these are recycled}
#' \item{"edge.lty": same than the previous argument but for line types; 1: plain, 2: dashed, 3: dotted, 4: dotdash, 5: longdash, 6: twodash}
#' \item{"font": an integer specifying the type of font for the labels: 1 (plain text), 2 (bold), 3 (italic, the default), or 4 (bold italic)}
#' \item{"cex": a numeric value giving the factor scaling of the tip and node labels (Character EXpansion). The default is to take the current value from the graphical parameters}
#' \item{"adj": a numeric specifying the justification of the text strings of the labels: 0 (left-justification), 0.5 (centering), or 1 (right-justification). This option has no effect if type="unrooted". If NULL (the default) the value is set with respect of direction (see details)}
#' \item{"srt": a numeric giving how much the labels are rotated in degrees (negative values are allowed resulting in clock-like rotation); the value has an effect respectively to the value of direction (see Examples). This option has no effect if type="unrooted"}
#' \item{"no.margin": a logical. If TRUE, the margins are set to zero and the plot uses all the space of the device}
#' \item{"label.offset": a numeric giving the space between the nodes and the tips of the phylogeny and their corresponding labels. This option has no effect if type="unrooted"}
#' \item{"rotate.tree": for "fan", "unrooted", or "radial" trees: the rotation of the whole tree in degrees (negative values are accepted}
#' }
#' A list of main parameters used in the function "ape::nodelabels":
#' \itemize{
#' \item{"text": a vector of mode character giving the text to be printed. By default, the labels for internal nodes (see "node.label"), that is, the bootstrap values associated with internal nodes}
#' \item{"node": a vector of mode numeric giving the numbers of the nodes where the text or the symbols are to be printed. By default, indexes for internal nodes, that is, (\eqn{Ntip}+1):(\eqn{Ntip}+\eqn{Nnode}), where \eqn{Ntip} is the number of tip nodes and \eqn{Nnode} for the number of internal nodes}
#' \item{"adj": one or two numeric values specifying the horizontal and vertical, respectively, justification of the text or symbols. By default, the text is centered horizontally and vertically. If a single value is given, this alters only the horizontal position of the text}
#' \item{"frame": a character string specifying the kind of frame to be printed around the text. This must be one of "rect" (the default), "circle", "none", or any unambiguous abbreviation of these}
#' \item{"cex": a numeric value giving the factor scaling of the tip and node labels (Character EXpansion). The default is to take the current value from the graphical parameters}
#' \item{"font": an integer specifying the type of font for the labels: 1 (plain text), 2 (bold), 3 (italic, the default), or 4 (bold italic)}
#' \item{"col": a character string giving the color to be used for the text or the plotting symbols; this is eventually recycled}
#' \item{"bg": a character string giving the color to be used for the background of the text frames or of the plotting symbols if it applies; this is eventually recycled. It can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), and "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta). Alternatively, any hyphen-separated HTML color names, e.g. "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}}
#' }
#' @export
#' @seealso \code{\link{visTreeBootstrap}}
#' @include visTreeBSclust.r
#' @examples    
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
#' colnames(data) <- paste(rep('S',10), seq(1:10), sep="")
#' data <- t(data)
#'
#' # 2) build neighbor-joining tree with bootstrap values and visualise it by default
#' tree_bs <- visTreeBootstrap(data)
#'
#' # 3) obtain clusters from a bootstrapped tree
#' res <- visTreeBSclust(tree_bs, bootstrap.cutoff=80)
#' ## hide tip labels and modify the font of internal node labels
#' res <- visTreeBSclust(tree_bs, bootstrap.cutoff=80, nodelabels.arg=list(cex=0.4), show.tip.label=FALSE)

visTreeBSclust <- function(tree_bs, bootstrap.cutoff=80, max.fraction=1, min.size=3, visTree=TRUE, plot.phylo.arg=NULL, nodelabels.arg=NULL, verbose=TRUE, ...)
{

    if (!is(tree_bs,"phylo")){
        stop("The function must apply to a 'phylo' object.\n")
    }

	#######################################
	## Define the tree struct dimensions
    Ntip <- length(tree_bs$tip.label)
    Nnode <- tree_bs$Nnode
    tip_index <- c(1:Ntip)
    node_index <- c((Ntip+1):(Ntip+Nnode))

	## Define the branches in highlight and their derived master branches (cluster MRCA)
	# specify the bootstrap value cutoff to highlight
	bs <- as.numeric(tree_bs$node.label)
	bs[is.na(bs)] <- 0 # replace NA with 0
	# get branches to be highlighted
	branchNames_highlight <- node_index[(bs>bootstrap.cutoff)]

    if(verbose){
    	message(sprintf("%d internal nodes with >= %d bootstrap value", length(branchNames_highlight), bootstrap.cutoff), appendLF=TRUE)
    }

	## most recent common ancestor (MRCA) for each pair of tips and nodes
    mrca_node <- ape::mrca(tree_bs, full=TRUE)
    ## connectivity linking each ancestor to its all children
    ## matrix of Nnode by (Ntip+Nnode)
    connectivity <- array(0, c(Nnode,Ntip+Nnode))
    for(i in 1:Nnode){
    	node_tmp <- i+Ntip
        child <- which(mrca_node[node_tmp,]==node_tmp, arr.ind=TRUE)
        connectivity[i,child] <- 1
        # exclude self
        connectivity[i,node_tmp] <- 0
    }
    rownames(connectivity) <- node_index
	
	## filter out those with too many leaves from being considered as master
	if(!is.null(max.fraction) & (max.fraction<=1)){
		branchNames_highlight_filter <- vector()
		k<-0
		for(i in 1:length(branchNames_highlight)){
			# only consider those having more than 5 leaves
			row_tmp <- branchNames_highlight[i]-Ntip
			sum_leaves <- sum(connectivity[row_tmp,1:Ntip])
			if (sum_leaves <= max.fraction*Ntip){
				k <- k+1
				branchNames_highlight_filter[k] <- branchNames_highlight[i]
			}
		}
		
		if(verbose){
			message(sprintf("%d internal nodes each with <= %d leaves", length(branchNames_highlight_filter), as.integer(max.fraction*Ntip)), appendLF=TRUE)
		}
		
	}else{
		branchNames_highlight_filter <- branchNames_highlight
	}
	
	## only those as the last common ancestor will be highlighted
	## only consider those having more than 4 leaves
	branchNames_master<-vector()
	k<-0
	for(i in 1:length(branchNames_highlight_filter)) {
    	# only consider those having more than 4 leaves
    	row_tmp <- branchNames_highlight_filter[i]-Ntip
    	sum_leaves <- sum(connectivity[row_tmp,1:Ntip])
    	if(sum_leaves >= min.size){
        	# only those the last common ancester will be highlighted
        	col <- branchNames_highlight_filter[i]
        	row <- which(connectivity[,col]==1)
        	tmp <- intersect(branchNames_highlight_filter-Ntip, row)
        	if(length(tmp)<1){
            	k <- k+1
            	branchNames_master[k] <- branchNames_highlight_filter[i]
        	}
    	}
	}
	branchNames_master <- sort(branchNames_master, decreasing=TRUE)
	
	if(verbose){
		message(sprintf("%d internal nodes each with >= %d leaves", length(branchNames_master), min.size), appendLF=TRUE)
	}

	## those highlighted not in master (cluster MRCA)
	branchNames_master_non <- setdiff(branchNames_highlight,branchNames_master)

	#######################################
	## Output cluster memberships
	output_samples <- tree_bs$tip.label
	output_clusters <- array("C0", Ntip)
	output_clans <- array(NA, Ntip)
	for (i in 1:length(branchNames_master)) {
    	# row in the connectivity
    	row <- branchNames_master[i]-Ntip
    	child_tip <- which(connectivity[row,1:Ntip]==1)
    	output_samples[child_tip] <- tree_bs$tip.label[child_tip]
    	output_clusters[child_tip] <- paste("C", i, sep = "")
    	
    	output_clans[child_tip] <- branchNames_master[i]
	}

	# List of output
	cluster_output_list <- list()
	cluster_output_list$Samples <- output_samples
	cluster_output_list$Clusters <- output_clusters
	cluster_output_list$Clans <- output_clans
 	output <- as.data.frame(cluster_output_list)
	# reverse row order of output
	res <- output[nrow(output):1,]
	
	if(verbose){
		message(sprintf("As a result, %d clusters are found", length(branchNames_master)), appendLF=TRUE)
	}
	
    if(visTree==TRUE){
        ########################################################################

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
    
        ################
        #plot.phylo.arg <- list(direction="upwards",lab4ut="axial")
    
        ## the default parameters for "plot.phylo"
        plot.phylo.arg.default <- list(
                    type = "phylogram",
                    direction = "rightwards",
                    lab4ut = "horizontal",
                    edge.color = "black",
                    edge.width = 1,
                    edge.lty = 1,
                    font = 3,
                    cex = 1,
                    adj = 0,
                    srt = 0,
                    no.margin = TRUE,
                    label.offset = 0,
                    rotate.tree = 0
                    )
    
        ## update parameters for "plot.phylo"
        plot.phylo.arg.default <- update_parameters(plot.phylo.arg.default, plot.phylo.arg)
    	
        ################
        ind <- which(!duplicated(res[, c(2,3)]) & !is.na(res[,3]))
    	CC <- res[ind, c(2,3)]
    	
        ## the default parameters for "nodelabels"
        nodelabels.arg.default <- list(
                    text = CC$Clusters,
                    node = CC$Clans,
                    adj = c(0.5, 0.5),
                    frame = "circle",
                    cex = 1, 
                    font = 4,
                    col = "black",
                    bg = "white-lightyellow-darkorange"
                    )
    
        ## update parameters for "nodelabels"
        nodelabels.arg.default <- update_parameters(nodelabels.arg.default, nodelabels.arg)
    
        ##################
        ape::plot.phylo(tree_bs, 
                        type = plot.phylo.arg.default$type,
                        direction = plot.phylo.arg.default$direction,
                        lab4ut = plot.phylo.arg.default$lab4ut,
                        edge.color = plot.phylo.arg.default$edge.color,
                        edge.width = plot.phylo.arg.default$edge.width,
                        edge.lty = plot.phylo.arg.default$edge.lty,
                        font = plot.phylo.arg.default$font,
                        cex = plot.phylo.arg.default$cex,
                        adj = plot.phylo.arg.default$adj,
                        srt = plot.phylo.arg.default$srt,
                        no.margin = plot.phylo.arg.default$no.margin,
                        label.offset = plot.phylo.arg.default$label.offset,
                        rotate.tree = plot.phylo.arg.default$rotate.tree,
                        ...
                        )
        ape::nodelabels(
                    text = nodelabels.arg.default$text,
                    node = nodelabels.arg.default$node,
                    adj = nodelabels.arg.default$adj,
                    frame = nodelabels.arg.default$frame,
                    cex = nodelabels.arg.default$cex, 
                    font = nodelabels.arg.default$font,
                    col = nodelabels.arg.default$col,
                    bg = visColormap(colormap=nodelabels.arg.default$bg)(101)[1 + as.numeric(tree_bs$node.label[nodelabels.arg.default$node-Ntip])]
                   )
   		
        ########################################################################
    }
	
	invisible(res)
}