apply_metric <- function(scores,
                         metric,
                         ...,
                         name = deparse(substitute(metric))) {
  force(name)
  
  list2(name = name, value = metric(scores, ...), options = dots_list(...))
}

# ad-hoc metric functions ---------------------------------------------------
standard_error <- function(x, cluster = NULL) {
  if (!is.null(cluster)) {
    return(standard_error_clustered(x = x, cluster = cluster))
  }

  # special case bernoulli draws
  if (all(x %in% c(0, 1))) {
    p <- mean(x)
    return(sqrt(p * (1 - p) / length(x)))
  }

  sd(x) / sqrt(length((x)))
}

# based on https://github.com/UKGovernmentBEIS/inspect_ai/blob/5b6502e6cb12f211551317a94622020cfd1a9f3b/src/inspect_ai/scorer/_metrics/std.py#L70
standard_error_clustered <- function(x, cluster, call = caller_env()) {
  if (!identical(length(x), length(cluster))) {
    cli::cli_abort(
      "{.arg cluster} must be the same length as {.arg x}.",
      call = call
    )
  }

  overall_mean <- mean(x)
  
  unique_clusters <- unique(cluster)
  cluster_count <- length(unique_clusters)

  
  clustered_variance <- 0
  for (cluster_id in unique_clusters) {
    cluster_data <- x[cluster == cluster_id]
    
    # calculate sum of all pairwise products of deviations from mean
    deviations <- cluster_data - overall_mean
    cluster_contribution <- sum(outer(deviations, deviations))
    clustered_variance <- clustered_variance + cluster_contribution
  }
  
  # apply finite cluster correction to unbias the variance estimate
  res <- 
    sqrt(clustered_variance * cluster_count / (cluster_count - 1)) / length(x)
  
  return(res)
}
