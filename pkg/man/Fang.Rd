\name{Fang}
\alias{Fang}
\alias{Fang.sampleinfo}
\alias{Fang.geneinfo}

\title{Human embryo gene expression dataset from Fang et al. (2010)}

\usage{
data(Fang)
}

\description{
Human embryo dataset contains gene expression levels (5441 genes and 18 embryo samples) from Fang et al. (2010).
}

\value{
    \item{Fang}{a gene expression matrix of 5441 genes x 18 samples, involving six successive stages, each with three replicates.}
    \item{Fang.sampleinfo}{a matrix containing the information of the 18 samples for the expression matrix \code{Fang}. The three columns correspond to the sample \code{Name}, \code{Stage}, and \code{Replicate}, respectively.}
    \item{Fang.geneinfo}{a matrix containing the information of the 5441 genes for the expression matrix \code{Fang}. The four columns correspond to the gene \code{AffyID}, \code{EntrezGene}, and \code{Symbol}, respectively.}
}

\references{
Fang et al. (2010). Transcriptome analysis of early organogenesis in human embryos. \emph{Developmental Cell}, 19(1):174-84.
}

\keyword{datasets}