\name{dtm_to_sparsem}
\alias{dtm_to_sparsem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
converts a tm DocumentTermMatrix or TermDocumentMatrix into a matrix.csr representation.
}
\description{
Converts a \pkg{tm} \code{DocumentTermMatrix} or \code{TermDocumentMatrix} into a \code{matrix.csr} representation to be used in the \code{\link{maxent}} and \code{\link{predict.maxent}} functions.
}
\usage{
dtm_to_sparsem(dtm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dtm}{
A \pkg{tm} \code{DocumentTermMatrix} or \code{TermDocumentMatrix} class.
}
}

\value{
A \code{matrix.csr} representation of the \code{DocumentTermMatrix} or \code{TermDocumentMatrix}.
}

\author{
Tim Jurka <tpjurka@ucdavis.edu>
}

\examples{
# LOAD LIBRARY
library(maxent)

# READ THE DATA, PREPARE THE CORPUS, and CREATE THE MATRIX
data <- read.csv(system.file("data/NYTimes.csv",package="maxent"))
corpus <- Corpus(VectorSource(data$Title))
matrix <- DocumentTermMatrix(corpus)

# CREATE A MATRIX.CSR (SPARSEM) REPRESENTATION
sparse <- dtm_to_sparsem(matrix)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
