# This is a demo for analysing RNA-seq dataset (along with edgeR)
#
# The dataset (<a href="RNAseq_counts.txt">RNAseq_counts.txt</a>) contains the RNA-seq counts for all human 62757 genes across 6 samples. These 6 samples have paired experimental design with 3 pairs (i.e. 3 cell lines 'cellline1', 'cellline2', 'cellline3'), each with before and after treatments (i.e. the control 'CON', and the Dex treatment 'DEX'). This paired design is able to reduce the effects of cell lines (e.g. cell line variances), and thus to increase the statistical power.
# Also provided is the file (<a href="RNAseq_geneinfo.txt">RNAseq_geneinfo.txt</a>) detailing 6 gene information, including 'EnsemblGeneID', 'GeneType', 'GeneName', 'Description', 'EntrezGeneID' and 'GeneLength'. 
#
# The first aim is to use the package 'edgeR' for identifying differentially expressed genes between the Dex treatment and the control, taking into account the paired design for cell lines.
# The second aim is to analyse/visualise differentially expressed genes using the package 'supraHex'.
###############################################################################

library(supraHex)

# Load or install packages (i.e. edgeR) specifically used in this demo
for(pkg in c("edgeR")){
    if(!require(pkg, character.only=T)){
        source("http://bioconductor.org/biocLite.R")
        biocLite(pkg)
        lapply(pkg, library, character.only=T)
    }
}

# Import data
## import RNA-seq counts data file <a href="RNAseq_counts.txt">RNAseq_counts.txt</a>
RNAseq_counts <- read.delim(file="http://supfam.org/supraHex/RNAseq_counts.txt", header=T, row.names=1) # a matrix of 62757 genes X 6 samples.
## import RNA-seq counts data file <a href="RNAseq_geneinfo.txt">RNAseq_geneinfo.txt</a>
RNAseq_geneinfo <- read.delim(file="http://supfam.org/supraHex/RNAseq_geneinfo.txt", header=T, row.names=1) # a matrix of 62757 genes X 5 gene information columns (plus the row names).


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Identify differentially expressed genes using the package 'edgeR'
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# (I) Create a DGEList object (edgeR's container for RNA-seq count data)
d_obj <- DGEList(counts=RNAseq_counts, genes=RNAseq_geneinfo)

## In edgeR, it is recommended to remove genes/features without at least 1 read/count per million (known as 'cpm') in n of the samples, where n is the size of the smallest group of replicates. In this case, n=3 but we set it to be 6 as the strictest filtering
cpms <- edgeR::cpm(d_obj$counts)
keep <- rowSums(cpms>=1) >= 6
d <- d_obj[keep,]

## Reset the library sizes
d$samples$lib.size <- colSums(d$counts)

## Estimate normalization factors using:
d <- calcNormFactors(d)

## Define the design matrix
cnames <- colnames(d)
cellline <- gsub("_.*", "", cnames, perl=T)
treatment <- gsub(".*_", "", cnames, perl=T)
targets <- data.frame(sample=cnames, cellline=cellline, treatment=treatment)
design <- model.matrix(~cellline+treatment, targets)
design

## Inspect the relationships between samples using a multidimensional scaling (MDS) plot to show the relationship between all pairs of samples
plotMDS(d, labels=colnames(d), col=c("red","darkgreen","blue")[factor(targets$cellline)], xlim=c(-2,2), ylim=c(-2,2))
### Note: this inspection clearly shows the variances between cell lines, calling for paired design test.

# (II) Do dispersion estimation and GLM (Generalized Linear Model) fitting
# Estimate the overall dispersion to get an idea of the overall level of biological variablility
d <- estimateGLMCommonDisp(d, design, verbose=T)

## Estimate dispersion values, relative to the design matrix, using the Cox-Reid (CR)-adjusted likelihood:
d2 <- estimateGLMTrendedDisp(d, design)
d2 <- estimateGLMTagwiseDisp(d2, design)

## Given the design matrix and dispersion estimates, fit a GLM to each feature:
f <- glmFit(d2, design)

# (III) Perform a likelihood ratio test
## Specify the difference of interest: DEX vs CON
contrasts <- rbind(c(0,0,0,1))

## Prepare the results
logFC <- matrix(nrow=nrow(d2), ncol=1)
PValue <- matrix(nrow=nrow(d2), ncol=1)
FDR <- matrix(nrow=nrow(d2), ncol=1)
tmp <- c("DEX_CON")
colnames(logFC) <- paste(tmp, '_logFC', sep='')
colnames(PValue) <- paste(tmp, '_PValue', sep='')
colnames(FDR) <- paste(tmp, '_FDR', sep='')
rownames(logFC) <- rownames(PValue) <- rownames(FDR) <- rownames(d2)

## Perform the test, calculating P-values and FDR (false discovery rate)
for(i in 1:nrow(contrasts)){   
    lrt <- glmLRT(f, contrast=contrasts[i,])
    tt <- topTags(lrt, n=nrow(d2), adjust.method="BH", sort.by="none")
    logFC[,i] <- tt$table$logFC
    PValue[,i] <- tt$table$PValue
    FDR[,i] <- tt$table$FDR
}

## MA plots for RNA-seq data
lrt <- glmLRT(f, contrast=contrasts[1,])
#lrt <- glmLRT(f, coef=4)
summary(de <- decideTestsDGE(lrt, adjust.method="BH", p.value=0.05))
detags <- rownames(d2)[as.logical(de)]
plotSmear(lrt, de.tags=detags)
### As you have seen, it plots the log-fold change (i.e., the log ratio of normalized expression levels between two experimental conditions (i.e. DEX vs CON) against the log counts per million (CPM). Those genes selected as differentially expressed (with a 5% false discovery rate) are highlighted as red dots

# (IV) Output edgeR results
## log counts per million (CPM) for each sample
cpms <- edgeR::cpm(d2,log=T,prior.count=2)
colnames(cpms) <- paste(colnames(d2), '_CPM', sep='')

## log ratio between the treatment vs the control (for each cell line)
odd_indexes <- seq(1,ncol(cpms),2)
even_indexes <- seq(2,ncol(cpms),2)
logFC_cpm <- cpms[,even_indexes] - cpms[,odd_indexes]
colnames(logFC_cpm) <- gsub("_CPM", "_CPM_logFC", colnames(logFC_cpm), perl=T)

## write into the file 'RNAseq_edgeR.txt'
out <- data.frame(EnsemblGeneID=rownames(d2$genes), d2$genes, cpms, logFC_cpm, logFC, PValue, FDR)
write.table(out, file="RNAseq_edgeR.txt", col.names=T, row.names=F, sep="\t", quote=F)


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Analyse differentially expressed genes using the package 'supraHex'
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# (I) Load the package and select differentially expressed genes identified by edgeR data
library(supraHex)
select_flag <- FDR<0.05
data <- logFC_cpm[select_flag, ]
## check the data dimension: how many genes are called significant
dim(data)

# (II) Train the supra-hexagonal map with input data only
sMap <- sPipeline(data, xdim=8, algorithm="sequential")
visHexMulComp(sMap, title.rotate=5, colormap="darkgreen-lightgreen-lightpink-darkred")
sWriteData(sMap, data, filename="RNAseq_edgeR.supraHex.txt")
## As you have seen, a figure displays the multiple components of trained map in a sample-specific manner. You also see that a txt file <a href="RNAseq_edgeR.supraHex.txt">RNAseq_edgeR.supraHex.txt</a> has been saved in your disk. The output file has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. gene clusters). You can also force the input data to be output (see below).
sWriteData(sMap, data, filename="RNAseq_edgeR.supraHex_2.txt", keep.data=T)

# (III) Visualise the map, including built-in indexes, data hits/distributions, distance between map nodes, and codebook matrix
visHexMapping(sMap, mappingType="indexes")
## As you have seen, the smaller hexagons in the supra-hexagonal map are indexed as follows: start from the center, and then expand circularly outwards, and for each circle increase in an anti-clock order.

visHexMapping(sMap, mappingType="hits")
## As you have seen, the number represents how many input data vectors (mutations) are hitting each hexagon, the size of which is proportional to the number of hits.

visHexMapping(sMap, mappingType="dist")
## As you have seen, map distance tells how far each hexagon is away from its neighbors, and the size of each hexagon is proportional to this distance.

visHexPattern(sMap, plotType="lines")
## As you have seen, line plot displays the patterns associated with the codebook matrix. If multple colors are given, the points are also plotted. When the pattern involves both positive and negative values, zero horizental line is also shown.

visHexPattern(sMap, plotType="bars")
## As you have seen, bar plot displays the patterns associated with the codebook matrix. When the pattern involves both positive and negative values, the zero horizental line is in the middle of the hexagon; otherwise at the top of the hexagon for all negative values, and at the bottom for all positive values.

# (IV) Perform partitioning operation on the map to obtain continuous clusters (i.e. gene meta-clusters) as they are different from gene clusters in an individual map node
sBase <- sDmatCluster(sMap)
myColor <- c("transparent", "black")
border.color <- myColor[sBase$bases%%2 + 1] ## the hexagon frame according to mete-clusters
visDmatCluster(sMap,sBase, gp=grid::gpar(cex=1.5, font=2, col="blue"), colormap="PapayaWhip-pink-Tomato", area.size=0.95, border.color=border.color)
sWriteData(sMap, data, sBase, filename="RNAseq_edgeR.supraHex_base.txt")
## As you have seen, each cluster is filled with the same continuous color, and the cluster index is marked in the seed node. An output txt file <a href="RNAseq_edgeR.supraHex_base.txt">RNAseq_edgeR.supraHex_base.txt</a>. This file has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. mutation clusters), and 3rd column for the cluster bases (i.e. mutation meta-clusters). You can also force the input data to be output.
sWriteData(sMap, data, sBase, filename="RNAseq_edgeR.supraHex_base_2.txt", keep.data=T)

output <- visDmatHeatmap(sMap, data, sBase, base.separated.arg=list(col="black"), base.legend.location="bottomleft", colormap="darkgreen-lightgreen-lightpink-darkred", KeyValueName="Log2(Ratio)", labRow=NA, keep.data=T, srtCol=20, cexCol=1.5)
## As you have seen, heatmap is used to visualise gene expression patterns seen within each meta-cluster/base. Row side bar indicates the gene meta-clusters/bases. The returned variable "output" (NOT a txt file) has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. gene clusters), and 3rd column for the cluster bases (i.e. gene meta-clusters). Note: it has rows in the same order as visualised in the heatmap

# (V) Reorder the sample-specific components of the map to delineate relationships between taxonomy
sReorder <- sCompReorder(sMap, metric="euclidean")
visCompReorder(sMap, sReorder, title.rotate=5, colormap="darkgreen-lightgreen-lightpink-darkred")
## As you have seen, reordered components of trained map is displayed. Each component illustrates a sample-specific map and is placed within a two-dimensional rectangular lattice. Across components/samples, genes with similar expression patterns are mapped onto the same position of the map. Geometric locations of components delineate relationships between components/samples, that is, samples with the similar gene expression profiles are placed closer to each other.
