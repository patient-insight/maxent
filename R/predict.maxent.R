classify_maxent <- function(feature_matrix,model) {
    sname <- if (length(unique(feature_matrix@ja)) > 1) "ja" else "ra"
    ja <- sapply(slot(feature_matrix, sname),toString)

    results <- maximumentropy$classify_samples(as.integer(feature_matrix@dimension[1]),
                                               as.integer(feature_matrix@dimension[2]),
                                               feature_matrix@ia,
                                               ja,
                                               feature_matrix@ra,
                                               model);

    labels <- as.vector(results[[1]])
    probabilities <- as.matrix(results[[2]],mode="numeric")
    colnames(probabilities) <- as.vector(results[[3]],mode="character")

    cbind(labels,probabilities)
}

predict.maxent <- function(object, newdata, ...) {
    classify_maxent(as.compressed.matrix(newdata), object@model)
}
