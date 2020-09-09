#' Build a stepped-wedge cluster randomised trial design binary treatment
#' indicator matrix
#'
#' \code{\link{design_sw}} builds a binary treatment indicator
#' \code{\link{matrix}} for a specified stepped-wedge cluster randomised
#' trial (SW-CRT) design, for subsequent use with \code{\link{draw_sw}}.
#'
#' @usage design_sw(clusters_per_wave = c(0, 1, 1, 1, 1),
#'           labels            = c("0", "1"),
#'           row_names         = 1:sum(clusters_per_wave),
#'           col_names         = 1:length(clusters_per_wave))
#' @param clusters_per_wave ...
#' @param labels ...
#' @param row_names ...
#' @param col_names ...
#' @return Returns the binary treatment indicator \code{\link{matrix}} for
#' the implied SW-CRT design. This can be passed to \code{\link{draw_sw}}.
#' @author Michael J Grayling (\email{michael.grayling@@newcastle.ac.uk})
#' @seealso \code{\link{draw_sw}}
#' @examples
#' # The default is a 'standard' SW-CRT design, with four clusters and five time
#' # periods
#' default_design <- design_sw()
#' # Specify a more complex design, where all clusters begin in the intervention
#' # condition, there is an unequal number of clusters per wave, and there are
#' # two trailing extra time periods with all clusters in the intervention
#' # condition. Also modify the row and column names
#' complex_design <- design_sw(clusters_per_wave = c(1, 2, 3, 2, 0, 0),
#'                             row_names         = paste0("Cluster ", 1:8),
#'                             col_names         = paste0("Time period ", 1:6))
#' @export
design_sw <- function(clusters_per_wave = c(0, 1, 1, 1, 1),
                      labels            = c("0", "1"),
                      row_names         = 1:sum(clusters_per_wave),
                      col_names         = 1:length(clusters_per_wave)) {

  ##### Check inputs ###########################################################

  check_clusters_per_wave(clusters_per_wave)
  check_labels(labels)
  check_names(row_names, col_names, clusters_per_wave)

  ##### Build design matrix ####################################################

  C                   <- sum(clusters_per_wave)
  Ti                  <- length(clusters_per_wave)
  X                   <- matrix(labels[1], C, Ti)
  c                   <- 1L
  for (j in 1:Ti) {
    if (clusters_per_wave[j] > 0) {
      temp            <- c + clusters_per_wave[j] - 1L
      X[c:temp, j:Ti] <- labels[2]
      c               <- temp + 1L
    }
  }
  rownames(X)         <- row_names
  colnames(X)         <- col_names

  ##### Output #################################################################

  X

}