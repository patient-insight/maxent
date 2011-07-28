predict.maxent <-
function(object, feature_matrix, ...) {
	classify_maxent <-
	function(feature_matrix,model) {	
		if (is.null(colnames(feature_matrix))) {
			features <- as.character(c(1:ncol(feature_matrix)));
		} else {
			features <- colnames(feature_matrix);
		}
		
		results <- maximumentropy$classify_samples(features,feature_matrix,model);
		
		labels <- as.vector(results[[1]],mode="integer")
		probabilities <- as.matrix(results[[2]],mode="numeric")
		colnames(probabilities) <- as.vector(results[[3]],mode="character")
		
		return(cbind(labels,probabilities));	
	}
	
	classify_maxent_sparse <-
	function(feature_matrix,model) {
		ja <- sapply(feature_matrix@ja,toString);
		results <- maximumentropy$classify_samples_sparse(as.integer(feature_matrix@dimension[1]),as.integer(feature_matrix@dimension[2]),feature_matrix@ia,ja,feature_matrix@ra,model);
		
		labels <- as.vector(results[[1]],mode="integer")
		probabilities <- as.matrix(results[[2]],mode="numeric")
		colnames(probabilities) <- as.vector(results[[3]],mode="character")
		
		return(cbind(labels,probabilities));
	}	

	if (is.matrix.csr(feature_matrix) == TRUE) {
		results <- classify_maxent_sparse(feature_matrix,object@model)
	} else if (is.matrix(feature_matrix) == TRUE) {
		if (rownames(summary(dimnames(feature_matrix)))[1] == "Docs") {
			results <- classify_maxent(feature_matrix,object@model)
		} else if (rownames(summary(dimnames(feature_matrix)))[1] == "Terms") {
			stop("ERROR: Matrix must be of class DocumentTermMatrix, not TermDocumentMatrix.")
		}
	} else {
		stop("ERROR: Classification data must be in matrix or matrix.csr (see package SparseM) format.")
	}
	
	return(results);
}