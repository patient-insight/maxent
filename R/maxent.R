maxent <-
function(feature_matrix,code_vector,feature_cutoff=0,gaussian_prior=0,inequality_constraints=0,set_heldout=0) {	
	train_maxent <-
	function(feature_matrix,code_vector,feature_cutoff=0,gaussian_prior=0,inequality_constraints=0,set_heldout=0) {
		if (is.null(colnames(feature_matrix))) {
			features <- as.character(c(1:ncol(feature_matrix)));
		} else {
			features <- colnames(feature_matrix);
		}
		
		code_vector <- sapply(code_vector,toString);
		maximumentropy$add_samples(code_vector,features,feature_matrix);
		model <- maximumentropy$train_model(feature_cutoff,gaussian_prior,inequality_constraints,set_heldout);
		
		return(model)
	}
	
	train_maxent_sparse <-
	function(feature_matrix,code_vector,feature_cutoff=0,gaussian_prior=0,inequality_constraints=0,set_heldout=0) {
		ja <- sapply(feature_matrix@ja,toString);
		code_vector <- sapply(code_vector,toString);
		maximumentropy$add_samples_sparse(as.integer(feature_matrix@dimension[1]),as.integer(feature_matrix@dimension[2]),code_vector,feature_matrix@ia,ja,feature_matrix@ra);
		model <- maximumentropy$train_model(feature_cutoff,gaussian_prior,inequality_constraints,set_heldout);
		
		return(model)
	}
	
	if (gaussian_prior > 0 && inequality_constraints > 0) {
		print("ERROR: Gaussian priors and inequality modeling cannot be used together.");
		return(NULL);
	}
	
	if (is.matrix.csr(feature_matrix) == TRUE) {
		model <- train_maxent_sparse(feature_matrix,code_vector,feature_cutoff,gaussian_prior,inequality_constraints,set_heldout)
	} else if (is.matrix(feature_matrix) == TRUE) {
		if (rownames(summary(dimnames(feature_matrix)))[1] == "Docs") {
			model <- train_maxent(feature_matrix,code_vector,feature_cutoff,gaussian_prior,inequality_constraints,set_heldout)
		} else if (rownames(summary(dimnames(feature_matrix)))[1] == "Terms") {
			stop("ERROR: Matrix must be of class DocumentTermMatrix, not TermDocumentMatrix.")
		} else {
			stop("ERROR: Not a valid term document matrix.")
		}
	} else {
		stop("ERROR: Training data must be in matrix or matrix.csr (see package SparseM) format.")
	}
	
	weights <- as.data.frame(cbind(model[[2]],model[[3]],model[[4]]))
	colnames(weights) <- c("Weight","Label","Feature")
	container <- new("maxent", model=model[[1]], weights=weights)
	
	return(container)
}