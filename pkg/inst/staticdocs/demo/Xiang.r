# This is a demo for arabidopsis embryo dataset from Xiang et al
# 
# This arabidopsis embryo expression dataset (available from <a href="http://www.ncbi.nlm.nih.gov/pubmed/21402797" target="21402797">http://www.ncbi.nlm.nih.gov/pubmed/21402797</a>) contains gene expression levels (3,625 genes and 7 embryo stages). Only genes with at least 2-fold changes (in any stage) as compared to the average over embryo stages are considered. These embryos are at: early embryonic stage (i.e. Zygote and Quadrant), mid-embryonic stage (i.e. Globular, Heart and Torpedo), and late stages from Bent to Mature.
###############################################################################

# (I) Load the package and import data
library(supraHex)
data(Xiang)
data <- Xiang # a matrix of 3,625 genes expressed in 7 samples

# (II) Train the supra-hexagonal map with input data only
sMap <- sPipeline(data)
visHexMulComp(sMap,title.rotate=10,colormap="darkblue-white-darkorange")
sWriteData(sMap, data, filename="Output_Xiang.txt") 
## As you have seen, a figure displays the multiple components of trained map in a sample-specific manner. You also see that a .txt file has been saved in your disk. The output file has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. gene clusters). You can also force the input data to be output; type ?sWriteData for details.

# (III) Visualise the map, including built-in indexes, data hits/distributions, distance between map nodes, and codebook matrix
visHexMapping(sMap, mappingType="indexes")
## As you have seen, the smaller hexagons in the supra-hexagonal map are indexed as follows: start from the center, and then expand circularly outwards, and for each circle increase in an anti-clock order.

visHexMapping(sMap, mappingType="hits")
## As you have seen, the number represents how many input data vectors are hitting each hexagon, the size of which is proportional to the number of hits.

visHexMapping(sMap, mappingType="dist")
## As you have seen, map distance tells how far each hexagon is away from its neighbors, and the size of each hexagon is proportional to this distance.

visHexPattern(sMap, plotType="lines")
## As you have seen, line plot displays the patterns associated with the codebook matrix. If multple colors are given, the points are also plotted. When the pattern involves both positive and negative values, zero horizental line is also shown.

visHexPattern(sMap, plotType="bars",colormap="rainbow",legend.cex=0.5)
## As you have seen, bar plot displays the patterns associated with the codebook matrix. When the pattern involves both positive and negative values, the zero horizental line is in the middle of the hexagon; otherwise at the top of the hexagon for all negative values, and at the bottom for all positive values.

# (IV) Perform partitioning operation on the map to obtain continuous clusters (i.e. gene meta-clusters) as they are different from gene clusters in an individual map node
sBase <- sDmatCluster(sMap, reindexSeed="svd")
visDmatCluster(sMap, sBase)
output <- sWriteData(sMap, data, sBase, filename="Output_base_Xiang.txt", keep.data=T)
## As you have seen, each cluster is filled with the same continuous color, and the cluster index is marked in the seed node. Although different clusters are coded using different colors (randomly generated), it is unavoidable to have very similar colors filling in neighbouring clusters. In other words, neighbouring clusters are visually indiscernible. In this confusing situation, you can rerun the command visDmatCluster(sMap, sBase) until neighbouring clusters are indeed filled with very different colors. An output .txt file has been saved in your disk. This file has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. gene clusters), and 3rd column for the cluster bases (i.e. gene meta-clusters). You can also force the input data to be output; type ?sWriteData for details.

# (V) Reorder the sample-specific components of the map to delineate relationships between samples
sReorder <- sCompReorder(sMap, metric="pearson")
visCompReorder(sMap,sReorder,title.rotate=10,colormap="darkblue-white-darkorange")
## As you have seen, reordered components of trained map is displayed. Each component illustrates a sample-specific map and is placed within a two-dimensional rectangular lattice. Across components/samples, genes with similar expression patterns are mapped onto the same position of the map. Geometric locations of components delineate relationships between components/samples, that is, samples with the similar expression profiles are placed closer to each other.

# (VI) Get understanding gene bases/clusters
output[1:5,]
# contruct data frame including 1st column for temporary index, 2nd for hexagon index, 3rd for base/cluster ID and the rest for their correspoinding data matrix 
hexagon <- output[,2]
base <- output[,3]
tmp <- data.frame(ind=1:nrow(output), hexagon, base, data)
# order by: first base, then hexagon
ordering <- tmp[order(base,hexagon),]$ind

# (VII) Visualise the matrix using heatmap
# The genes are ordered according to the base/cluster memberships
D <- data[ordering, ]
bases <- base[ordering]
# prepare colors for the row sidebar of heatmap
# color for bases/clusters
lvs <- unique(bases)
lvs_color <- visColormap(colormap="jet")(length(lvs))
lvs_color <- visColoralpha(lvs_color, alpha=0.8) # add transparent (alpha) into colors
col_bases <- sapply(bases, function(x) lvs_color[x==lvs])
RowSideColors <- matrix(col_bases, nrow=1)
rownames(RowSideColors) <- c("Clusters")
# heatmap embeded with sidebars annotating gene cluster memberships
colormap <- "darkblue-white-darkorange"
visHeatmapAdv((D), Rowv=F, Colv=F, KeyValueName="Log2(Ratio)", colormap=colormap, RowSideColors=RowSideColors, labRow=NA)
# add legend
legend_txt <- paste(rep("Base",length(lvs)), lvs, sep=" ")
legend("bottomleft", legend=legend_txt, col=lvs_color, lty=1, lwd=5, cex=0.6, box.col="transparent", horiz=F)
# add separated lines between bases
sep_index <- sapply(unique(bases), function(x) which(bases[length(bases):1]==x)[1])
sep_index <- sep_index[1:length(sep_index)-1]
visHeatmapAdv((D), Rowv=F, Colv=F, KeyValueName="Log2(Ratio)", colormap=colormap, RowSideColors=RowSideColors, add.expr=graphics::abline(h=sep_index-0.5,lty=5), labRow=NA)
