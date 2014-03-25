\name{Golub}
\alias{Golub}

\title{Leukemia gene expression dataset from Golub et al. (1999)}

\usage{
data(Golub)
}

\description{
Leukemia dataset (learning set) contains gene expression levels (3051
genes and 38 patient samples) from Golub et al. (1999). This dataset
has been pre-processed: capping into floor of 100 and ceiling of 16000;
filtering by exclusion of genes with \eqn{max/min<=5} or
\eqn{max-min<=500}, where max and min refer respectively to the maximum
and minimum intensities for a particular gene across mRNA samples;
2-base logarithmic transformation.}

\value{
\itemize{
\item{\code{Golub}: a gene expression matrix of 3051 genes x 38
samples. These samples include 11 acute myeloid leukemia (AML) and 27
acute lymphoblastic leukemia (ALL) which can be further subtyped into
19 B-cell ALL and 8 T-cell ALL.}
}
}

\references{
Golub et al. (1999). Molecular classification of cancer: class
discovery and class prediction by gene expression monitoring,
\emph{Science}, Vol. 286:531-537.
\url{http://www-genome.wi.mit.edu/MPR/}\cr}

\keyword{datasets}
