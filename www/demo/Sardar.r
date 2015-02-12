# This is a demo for human cell type evolutionary profile dataset from Sardar et al
# 
# This dataset (available from <a href="http://www.ncbi.nlm.nih.gov/pubmed/24692656" target="24692656">http://www.ncbi.nlm.nih.gov/pubmed/24692656</a>) contains protein innovation usage (PIU) evolutionary profiles for 492 unique cell types, cell lines and tissues (in short, 492 cell types) over the human evolutionary lineage (13 key phyletic divisions in the NCBI taxonomy). These 13 taxonomy/divisions are (from human towards cellular organisms): "Human", "Theria (Live birth Mammals)", "Mammalia (Placental mammals, Marsupials and Monotremes)", "Amniota (Four limbed vertebrates with terrestrial eggs)", "Euteleostomi (Bony vertebrates)", "Chordata (Have a notochord/spinal column)", "Deuterostomia (Mouth comes second, after anus)", "Coelomata (Body cavity forming)", "Bilateria (Bilateral symmetry forming)", "Eumetazoa (True tissue forming animals)", "Opisthokonta (Animal-like and Fungi-like)", "Eukaryota", and "Cellular organisms".
#
# The dataset is stored in a form of matrix of 492 cell types X 13 taxonomy/divisions.
# The purpose of this demo is to group/cluster cell types with similar evolutionary profiles (over human ancestry) using self-organising algorithm.
# With the package 'supraHex', users can also easily visualise results at each step of the analysis.
###############################################################################

# (I) Load the package and import data
library(supraHex)
## import data file <a href="Sardar_PIU.txt">Sardar_PIU.txt</a>
data <- read.table(file="http://supfam.org/supraHex/Sardar_PIU.txt", header=T, row.names=1, sep="\t", check.names=F) # PIU matrix of 492 cell types X 13 taxonomy
## check data dimensions and types
dim(data)
str(data)

# (II) Train the supra-hexagonal map with input data only
sMap <- sPipeline(data)
visHexMulComp(sMap, title.rotate=5, title.xy=c(0.25,1), gp=grid::gpar(cex=0.6), zlim=c(-2,2), colormap="jet")
sWriteData(sMap, data, filename="Output_Sardar_PIU.txt")
## As you have seen, a figure displays the multiple components of trained map in a sample-specific manner. You also see that a txt file <a href="Output_Sardar_PIU.txt">Output_Sardar_PIU.txt</a> has been saved in your disk. The output file has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. cell type clusters). You can also force the input data to be output (see below).
sWriteData(sMap, data, filename="Output_Sardar_PIU_2.txt", keep.data=T)

# (III) Visualise the map, including built-in indexes, data hits/distributions, distance between map nodes, and codebook matrix
visHexMapping(sMap, mappingType="indexes")
## As you have seen, the smaller hexagons in the supra-hexagonal map are indexed as follows: start from the center, and then expand circularly outwards, and for each circle increase in an anti-clock order.

visHexMapping(sMap, mappingType="hits")
## As you have seen, the number represents how many input data vectors (cell types) are hitting each hexagon, the size of which is proportional to the number of hits.

visHexMapping(sMap, mappingType="dist")
## As you have seen, map distance tells how far each hexagon is away from its neighbors, and the size of each hexagon is proportional to this distance.

visHexPattern(sMap, plotType="lines")
## As you have seen, line plot displays the patterns associated with the codebook matrix. If multple colors are given, the points are also plotted. When the pattern involves both positive and negative values, zero horizental line is also shown.

visHexPattern(sMap, plotType="bars")
## As you have seen, bar plot displays the patterns associated with the codebook matrix. When the pattern involves both positive and negative values, the zero horizental line is in the middle of the hexagon; otherwise at the top of the hexagon for all negative values, and at the bottom for all positive values.

# (IV) Perform partitioning operation on the map to obtain continuous clusters (i.e. cell type meta-clusters) as they are different from cell type clusters in an individual map node
sBase <- sDmatCluster(sMap)
visDmatCluster(sMap, sBase)
sWriteData(sMap, data, sBase, filename="Output_base_Sardar_PIU.txt")
## As you have seen, each cluster is filled with the same continuous color, and the cluster index is marked in the seed node. Although different clusters are coded using different colors (randomly generated), it is unavoidable to have very similar colors filling in neighbouring clusters. In other words, neighbouring clusters are visually indiscernible. In this confusing situation, you can rerun the command visDmatCluster(sMap, sBase) until neighbouring clusters are indeed filled with very different colors. An output txt file <a href="Output_base_Sardar_PIU.txt">Output_base_Sardar_PIU.txt</a>. This file has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. cell type clusters), and 3rd column for the cluster bases (i.e. cell type meta-clusters). You can also force the input data to be output.
sWriteData(sMap, data, sBase, filename="Output_base_Sardar_PIU_2.txt", keep.data=T)

output <- visDmatHeatmap(sMap, data, sBase, base.separated.arg=list(col="black"), base.legend.location="bottomleft", colormap="jet", KeyValueName="PIU z-score", labRow=NA, keep.data=T, srtCol=20, cexCol=0.8)
## As you have seen, heatmap is used to visualise patterns seen in cell types within each meta-cluster/base. Row side bar indicates the cell type meta-clusters/bases. The returned variable "output" (NOT a txt file) has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. cell type clusters), and 3rd column for the cluster bases (i.e. cell type meta-clusters). Note: it has rows in the same order as visualised in the heatmap

# (V) Reorder the taxonomy-specific components of the map to delineate relationships between taxonomy
sReorder <- sCompReorder(sMap, metric="euclidean")
visCompReorder(sMap, sReorder, title.rotate=5, title.xy=c(0.25,1), gp=grid::gpar(cex=0.6),zlim=c(-2,2), colormap="jet")
## As you have seen, reordered components of trained map is displayed. Each component illustrates a taxonomy-specific map and is placed within a two-dimensional rectangular lattice. Across components/taxonomy, cell types with similar patterns are mapped onto the same position of the map. Geometric locations of components delineate relationships between components/taxonomy, that is, taxonomy with the similar profiles are placed closer to each other.

# (VI) Build and visualise the bootstrapped tree
tree_bs <- visTreeBootstrap(t(data), nodelabels.arg=list(cex=0.6))
## As you have seen, neighbour-joining tree is constructed based on pairwise euclidean distance matrices between samples. The robustness of tree branching is evaluated using bootstraping. In internal nodes (also color-coded), the number represents the proportion of bootstrapped trees that support the observed internal branching. The higher the number, the more robust the tree branching. 100 means that the internal branching is always observed by resampling characters.
