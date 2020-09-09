check_belong            <- function(value, name, allowed) {
  if (!is.vector(value)) {
    stop(name, " must be a vector")
  }
  for (i in 1:length(value)) {
    if (!(value[i] %in% allowed)) {
      stop(name, " must contain values only in ",
           paste(allowed, collapse = ", "))
    }
  }
}

check_character         <- function(input, name) {
  if (any(!is.character(input), length(input) > 1)) {
    stop(name, " must be a character string")
  }
}

check_clusters_per_wave <- function(clusters_per_wave) {
  if (any(clusters_per_wave < 0, clusters_per_wave%%1 != 0,
          sum(clusters_per_wave) <= 0)) {
    stop("clusters_per_wave must be a vector of integers that are greater than",
         " or equal to zero, with a strictly positive overall sum")
  }
}

check_colours           <- function(colours, design) {
  unique_labels    <- unique(as.vector(as.matrix(design)))
  if (missing(colours)) {
    colours        <- viridis::viridis(length(unique_labels))
    names(colours) <- unique_labels
  } else {
    if (any(!is.vector(colours), !is.character(colours),
            !all(names(colours) %in% unique_labels),
            length(colours) != length(unique_labels))) {
      stop("colours must be a named character vector, where the names match ",
           "the unique elements in design")
    }
  }
  colours
}

check_cp_contents       <- function(cp_contents, design) {
  if (all(!is.matrix(design), !is.data.frame(design),
          nrow(design) != nrow(cp_contents),
          ncol(design) != ncol(cp_contents))) {
    stop("cp_contents must be a matrix or a data.frame, with dimensions equal ",
         "to those of design")
  }
  if (is.matrix(cp_contents)) {
    cp_contents <- as.data.frame(cp_contents)
  }
  cp_contents
}

check_design            <- function(design) {
  if (all(!is.matrix(design), !is.data.frame(design))) {
    stop("design must be either a matrix or a data.frame")
  }
  if (is.matrix(design)) {
    design <- as.data.frame(design)
  }
  design
}

check_keys              <- function(key_interventions, key_time_periods,
                                    design) {
  unique_labels <- unique(as.vector(as.matrix(design)))
  if (!missing(key_interventions)) {
    if (any(!is.vector(key_interventions),
            !all(names(key_interventions) %in% unique_labels),
            length(key_interventions) != length(unique_labels))) {
      stop("key_interventions must be a named vector, where the names match ",
           "the unique elements in design")
    }
  }
  if (!missing(key_time_periods)) {
    if (any(length(key_time_periods) != ncol(design))) {
      stop("key_time_periods must be a vector of length equal to the number of",
           " columns in design")
    }
  }
}

check_labels            <- function(labels) {
  if (any(!is.vector(labels), length(labels) != 2)) {
    stop("labels must be a vector of length two")
  }
}

check_logical           <- function(input, name) {
  if (any(!is.logical(input), length(input) > 1)) {
    stop(name, " must be a logical variable")
  }
}

check_names             <- function(row_names, col_names, clusters_per_wave) {
  if (any(!is.vector(row_names), length(row_names) != sum(clusters_per_wave))) {
    stop("row_names must be a vector of length equal to sum(clusters_per_wave)")
  }
  if (any(!is.vector(col_names),
          length(col_names) != length(clusters_per_wave))) {
    stop("col_names must be a vector of length equal to ",
         "length(clusters_per_wave)")
  }
}
