\name{Xiang}
\alias{Xiang}

\title{Arabidopsis embryo gene expression dataset from Xiang et al.
(2011)}

\usage{
data(Xiang)
}

\description{
Arabidopsis embryo dataset contains gene expression levels (3625 genes
and 7 embryo samples) from Xiang et al. (2011).
This dataset has been pre-processed: capping into floor of intensity
777.6; 2-base logarithmic transformation; row/gene centering; and
keeping genes with at least 2-fold changes (in any stage) as compared
to the average over embryo stages.}

\value{
\itemize{
\item{\code{Xiang}: a gene expression matrix of 3625 genes x 7 stage
samples. These embryo stages are: zygote, quadrant, globular, heart,
torpedo, bent, and mature.}
}
}

\references{
Xiang et al. (2011) Genome-wide analysis reveals gene expression and
metabolic network dynamics during embryo development in Arabidopsis.
\emph{Plant Physiol}, 156(1):346-356.
}

\keyword{datasets}
