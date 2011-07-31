as.compressed.matrix <- function(DocumentTermMatrix) {
	if (pmatch("TermDocumentMatrix",class(DocumentTermMatrix),nomatch=0) > 0) DocumentTermMatrix <- t(DocumentTermMatrix);
	
	ia <- c(1);
	for (n in 1:dim(DocumentTermMatrix)[1]) {
		el <- sum(DocumentTermMatrix$i == n)+ia[length(ia)];
		ia <- append(ia,el);
	}
	
	matrix <- new("matrix.csr",ra=as.numeric(DocumentTermMatrix$v),ja=DocumentTermMatrix$j,ia=as.integer(ia),dimension=dim(DocumentTermMatrix));
	
	return(matrix);
}