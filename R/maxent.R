train_maxent <- function(feature_matrix,code_vector,l1_regularizer=0.0,l2_regularizer=0.0,use_sgd=FALSE,sgd_iter=30,sgd_eta0=1.0,sgd_alpha=0.85,set_heldout=0) {

    sname <- if (length(unique(feature_matrix@ja)) > 1) "ja" else "ra"
    ja <- sapply(slot(feature_matrix, sname),toString)
    code_vector <- sapply(code_vector,toString);
    maximumentropy$add_samples(as.integer(feature_matrix@dimension[1]),as.integer(feature_matrix@dimension[2]),code_vector,feature_matrix@ia,ja,feature_matrix@ra);
    model <- maximumentropy$train_model(l1_regularizer,l2_regularizer,use_sgd,sgd_iter,sgd_eta0,sgd_alpha,set_heldout);

    return(model);
}

maxent <- function(feature_matrix, code_vector, l1_regularizer=0.0, l2_regularizer=0.0, use_sgd=FALSE, set_heldout=0, verbose=FALSE) {
    suppressWarnings(sink())
    feature_matrix <- as.compressed.matrix(feature_matrix);

	if (l1_regularizer > 0 && l2_regularizer > 0) {
		stop("ERROR: L1 and L2 regularization cannot be used together.")
		return(NULL);
	}

    if (l2_regularizer > 0 && use_sgd==TRUE) {
        stop("ERROR: L2 regularization is currently not supported in SGD mode.")
    }

    if (length(unique(code_vector)) > 255) {
        stop("ERROR: Too many types of labels (>255 unique labels).");
    }

    if (verbose == FALSE) {
        if(.Platform$OS.type == "unix") {
            sink("/dev/null")
        } else {
            sink("NUL")
        }
    }

    model <- train_maxent(feature_matrix,code_vector,l1_regularizer,l2_regularizer,use_sgd,set_heldout);

	weights <- data.frame(Weight=model[[2]], Label=model[[3]], Feature=model[[4]], stringsAsFactors=FALSE);

	container <- new("maxent", model=model[[1]], weights=weights);

    suppressWarnings(sink())

	container
}
