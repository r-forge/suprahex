# This is a demo for leukemia patient dataset from Golub et al
# 
# This leukemia patient expression dataset (the learning set, available from <a href="http://www.ncbi.nlm.nih.gov/pubmed/10521349" target="10521349">http://www.ncbi.nlm.nih.gov/pubmed/10521349</a>) contains an expression matrix of 3,051 genes X 38 samples, involving two types of leukemia: 11 acute myeloid leukemia (AML) and 27 acute lymphoblastic leukemia (ALL). These 27 ALL are further subtyped into 19 B-cell ALL (ALL_B) and 8 T-cell ALL (ALL_T).
###############################################################################

# (I) Load the package and import data
library(supraHex)
data(Golub)
data <- Golub # a matrix of 3,051 genes expressed in 38 samples

# (II) Train the supra-hexagonal map with input data only
sMap <- sPipeline(data)
visHexMulComp(sMap,title.rotate=10,colormap="darkgreen-lightgreen-lightpink-darkred")
sWriteData(sMap, data, filename="Output_Golub.txt")
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

visHexPattern(sMap, plotType="bars")
## As you have seen, bar plot displays the patterns associated with the codebook matrix. When the pattern involves both positive and negative values, the zero horizental line is in the middle of the hexagon; otherwise at the top of the hexagon for all negative values, and at the bottom for all positive values.

# (IV) Perform partitioning operation on the map to obtain continuous clusters (i.e. gene meta-clusters) as they are different from gene clusters in an individual map node
sBase <- sDmatCluster(sMap)
visDmatCluster(sMap, sBase)
sWriteData(sMap, data, sBase, filename="Output_base_Golub.txt")
## As you have seen, each cluster is filled with the same continuous color, and the cluster index is marked in the seed node. Although different clusters are coded using different colors (randomly generated), it is unavoidable to have very similar colors filling in neighbouring clusters. In other words, neighbouring clusters are visually indiscernible. In this confusing situation, you can rerun the command visDmatCluster(sMap, sBase) until neighbouring clusters are indeed filled with very different colors. An output .txt file has been saved in your disk. This file has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. gene clusters), and 3rd column for the cluster bases (i.e. gene meta-clusters). You can also force the input data to be output; type ?sWriteData for details.

# (V) Reorder the sample-specific components of the map to delineate relationships between samples
sReorder <- sCompReorder(data,metric="pearson") # see Figure 8
visCompReorder(sMap,sReorder,title.rotate=15,colormap="darkgreen-lightgreen-lightpink-darkred")
## As you have seen, reordered components of trained map is displayed. Each component illustrates a sample-specific map and is placed within a two-dimensional rectangular lattice. Across components/samples, genes with similar expression patterns are mapped onto the same position of the map. Geometric locations of components delineate relationships between components/samples, that is, samples with the similar expression profiles are placed closer to each other.

# (VI) Build and visualise the bootstrapped tree
D <- t(data)
rownames(D) <- paste(rownames(D), 1:nrow(D), sep=".") # temporally make sure the row names are unique
tree_bs <- visTreeBootstrap(D, nodelabels.arg=list(cex=0.7))
## As you have seen, neighbour-joining tree is constructed based on pairwise euclidean distance matrices between samples. The robustness of tree branching is evaluated using bootstraping. In internal nodes (also color-coded), the number represents the proportion of bootstrapped trees that support the observed internal branching. The higher the number, the more robust the tree branching. 100 means that the internal branching is always observed by resampling characters/genes. 

# (VII) Visualise the matrix using heatmap
# The samples are ordered according to the neighbour-joining tree
flag <- match(tree_bs$tip.label, rownames(D))
rownames(D) <- sub("\\.\\d+$", "", rownames(D)) # restore the original names
D <- D[flag,]
# prepare colors for the column sidebar of heatmap
# color for AML/ALL types
types <- sub("_.*","",rownames(D))
lvs <- unique(types)
lvs_color <- visColormap(colormap="darkblue-darkorange")(length(lvs))
col_types <- sapply(types, function(x) lvs_color[x==lvs])
# color for ALL subtypes
subtypes <- sub(".*_","",rownames(D))
lvs <- unique(subtypes)
lvs_color <- visColormap(colormap="gray-black")(length(lvs))
col_subtypes <- sapply(subtypes, function(x) lvs_color[x==lvs])
# combine both color vectors
ColSideColors <- cbind(col_subtypes,col_types)
colnames(ColSideColors) <- c("ALL subtypes", "AML/ALL types")
# heatmap embeded with sidebars annotating samples
visHeatmapAdv(t(D), Rowv=T, Colv=F, dendrogram="none", colormap="darkgreen-lightgreen-lightpink-darkred", ColSideColors=ColSideColors, ColSideHeight=0.4, ColSideLabelLocation="left", labRow=NA)
