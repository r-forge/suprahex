# This is a demo for multilayer omics dataset from Hiratani et al
# 
# This multilay omics dataset (available from <a href="http://www.ncbi.nlm.nih.gov/pubmed/19952138" target="19952138">http://www.ncbi.nlm.nih.gov/pubmed/19952138</a>) involves genome-wide replication-timing profiles of 22 cell lines from early mouse embryogenesis. These cell lines can be categorised into: 1) pluripotent cells, including ESCs (ESC_46C, ESC_D3 and ESC_TT2) and iPSCs (iPSC, iPSC_1D4 and iPSC_2D4); 2) partially-reprogrammed iPSCs (piPSC_1A2, piPSC_1B3 and piPSC_V3); 3) early epiblast (EPL and EMB3_D3); 4) late epiblast (EpiSC5 and EpiSC7); 5) Ectoderm (EBM6_D3, EBM9_D3, NPC_46C and NPC_TT2); 6) Mesoderm and Endoderm; and 7) late Mesoderm (Myoblast, MEF_female and MEF_male).
#
# The dataset is extracted for RefSeq gene TSS locations, including:
## RT: a replication timing matrix of 17,292 genes X 22 samples;
## CpG: a matrix of 17,292 genes X 1 containing gene additional information on promoter CpG classification (see <a href="http://www.ncbi.nlm.nih.gov/pubmed/17603471" target="17603471">http://www.ncbi.nlm.nih.gov/pubmed/17603471</a>), with '1' for HCP (high CpG density promoters), '-1' for LCP (low CpG density promoters), '0' for ICP (intermediate CpG density promoters), and 'NA' for unclassified;
## EX: an expression matrix of 17,292 genes X 8 samples, and samples include pluripotent cells (ESC_D3); early epiblast (EMB3_D3); late epiblast (EpiSC7); Ectoderm (EBM6_D3 and EBM9_D3); Mesoderm and Endoderm.
###############################################################################

# (I) Load the package and import data
library(supraHex)
URL <- url("http://supfam.org/supraHex/Hiratani_TableS1.Rda")
load(URL)
close(URL)
ls() # you should see three variables: 'RT', 'CpG' and 'EX'
data <- RT # a replication timing matrix of 17,292 genes X 22 samples

# (II) Train the supra-hexagonal map with input data only
sMap <- sPipeline(data)
visHexMulComp(sMap, title.rotate=5, gp=grid::gpar(cex=0.8), zlim=c(-2,2), colormap="darkblue-white-darkorange")
sWriteData(sMap, data, filename="Output_Hiratani_TableS1.txt")
## As you have seen, a figure displays the multiple components of trained map in a sample-specific manner. You also see that a .txt file has been saved in your disk. The output file has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. gene clusters). You can also force the input data to be output; type ?sWriteData for details.

# (III) Visualise the map, including built-in indexes, data hits/distributions, distance between map nodes, and codebook matrix
visHexMapping(sMap, mappingType="indexes", gp=grid::gpar(cex=0.5))
## As you have seen, the smaller hexagons in the supra-hexagonal map are indexed as follows: start from the center, and then expand circularly outwards, and for each circle increase in an anti-clock order.

visHexMapping(sMap, mappingType="hits", gp=grid::gpar(cex=0.5))
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
sWriteData(sMap, data, sBase, filename="Output_base_Hiratani_TableS1.txt")
## As you have seen, each cluster is filled with the same continuous color, and the cluster index is marked in the seed node. Although different clusters are coded using different colors (randomly generated), it is unavoidable to have very similar colors filling in neighbouring clusters. In other words, neighbouring clusters are visually indiscernible. In this confusing situation, you can rerun the command visDmatCluster(sMap, sBase) until neighbouring clusters are indeed filled with very different colors. An output .txt file has been saved in your disk. This file has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. gene clusters), and 3rd column for the cluster bases (i.e. gene meta-clusters). You can also force the input data to be output; type ?sWriteData for details.

# (V) Reorder the sample-specific components of the map to delineate relationships between samples
sReorder <- sCompReorder(sMap, metric="mi")
visCompReorder(sMap, sReorder, title.rotate=10, gp=grid::gpar(cex=0.6,font=2),zlim=c(-2,2), colormap="darkblue-white-darkorange")
## As you have seen, reordered components of trained map is displayed. Each component illustrates a sample-specific map and is placed within a two-dimensional rectangular lattice. Across components/samples, genes with similar expression patterns are mapped onto the same position of the map. Geometric locations of components delineate relationships between components/samples, that is, samples with the similar expression profiles are placed closer to each other.

# (VI) Overlay the CpG additional data to the trained map to view the distribution of CpG data
additional_HCP <- as.numeric(CpG==1 & !is.na(CpG))
additional_LCP <- as.numeric(CpG==-1 & !is.na(CpG))
additional <- cbind(additional_HCP, additional_LCP)
colnames(additional) <- c("HCP","LCP")
sOverlay <- sMapOverlay(sMap=sMap, data=data, additional=additional) 
visHexMulComp(sOverlay, colormap="lightyellow-darkred", zlim=c(0, signif(max(sOverlay$codebook),1)))

# (VII) Overlay the expression additional data to the trained map to view the distribution of expression data, which is further used to explore relationships between samples
sOverlay <- sMapOverlay(sMap=sMap, data=data, additional=EX)
sReorder <- sCompReorder(sOverlay, metric="none") # here reorder samples based on overlaid distribution data rather than expression data
visCompReorder(sOverlay, sReorder, title.rotate=10, colormap="darkgreen-lightgreen-lightpink-darkred")

