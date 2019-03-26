as.compressed.matrix <- function(X) {
    if (inherits(X, "matrix.csr") { return(X); }
    else if (inherits(X, "TermDocumentMatrix") { X <- t(X); }
    else if (inherits(X, "DocumentTermMatrix") { NULL }  #FALLTHROUGH TO BELOW
    # Three below cases can be handled by else
    #else if (inherits(X, "Matrix") { return(as.matrix.csr(X)); }
    #else if (inherits(X, "data.frame") { return(as.matrix.csr(as.matrix(X))); }
    #else if (inherits(X, "matrix") { return(as.matrix.csr(X)); }
    else {
    tryCatch(return(as.matrix.csr(as.matrix(X))),
             error=function(e) stop(AS_ERR_MSG));
    }

    ia <- cumsum(table(factor(X$i, 1:N)))

    new("matrix.csr",
        ra=as.numeric(X$v),
        ja=X$j,
        ia=as.integer(ia),
        dimension=dim(X))
}

AS_ERR_MSG <- "Data must be encapsulated using one of the following classes: X or TermDocumentMatrix (package tm), Matrix (package Matrix), matrix.csr (SparseM), data.frame, or matrix"
