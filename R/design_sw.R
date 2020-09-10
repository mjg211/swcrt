#' Build a stepped-wedge cluster randomised trial design binary treatment
#' indicator matrix
#'
#' \code{\link{design_sw}} builds a binary treatment indicator
#' \code{\link{matrix}} for a specified conventional (i.e., two treatment
#' conditions, say control and experimental interventions) stepped-wedge cluster
#' randomised trial (SW-CRT) design, for subsequent use with
#' \code{\link{draw_sw}}.
#'
#' @usage design_sw(clusters_per_time_period = c(0, 1, 1, 1, 1),
#'           labels                   = c("0", "1"),
#'           row_names                =
#'             1:sum(clusters_per_time_period),
#'           col_names                 =
#'             1:length(clusters_per_time_period))
#' @param clusters_per_time_period A \code{\link{numeric}} \code{\link{vector}};
#' element \code{clusters_per_time_period[i]} indicates the number of clusters
#' that switch to the experimental intervention in time period \code{i}. The
#' \code{\link{length}} of \code{clusters_per_time_period} thus indicates the
#' number of time periods. Defaults to \code{c(0, 1, 1, 1, 1)}.
#' @param labels A \code{\link{vector}} of \code{\link{length}} two, giving
#' labels for the two intervention conditions. Defaults to \code{c("0", "1")}.
#' @param row_names A \code{\link{vector}}, giving \code{\link{rownames}} to add
#' to the produced design. Must have \code{\link{length}} equal to
#' \code{\link{sum}(clusters_per_time_period)}.
#' @param col_names A \code{\link{vector}}, giving \code{\link{colnames}} to add
#' to the produced design. Must have \code{\link{length}} equal to
#' \code{\link{length}(clusters_per_time_period)}.
#' @return Returns the binary treatment indicator matrix, in
#' \code{\link{matrix}} form, for the implied SW-CRT design. This can be passed
#' to \code{\link{draw_sw}}.
#' @author Michael J Grayling (\email{michael.grayling@@newcastle.ac.uk})
#' @seealso \code{\link{draw_sw}}
#' @examples
#' # The default is a 'standard' SW-CRT design, with four clusters and five time
#' # periods
#' default_design <- design_sw()
#' # Specify a more complex design, where all clusters begin in the intervention
#' # condition, there is an unequal number of clusters who switch per time
#' # period, and there are two trailing extra time periods with all clusters in
#' # the intervention condition. Also modify the row and column names
#' complex_design <- design_sw(clusters_per_time_period = c(1, 2, 3, 2, 0, 0),
#'                             row_names                = paste("Cluster", 1:8),
#'                             col_names                = paste("Time period",
#'                                                              1:6))
#' @export
design_sw <- function(clusters_per_time_period = c(0, 1, 1, 1, 1),
                      labels                   = c("0", "1"),
                      row_names                =
                        1:sum(clusters_per_time_period),
                      col_names                =
                        1:length(clusters_per_time_period)) {

  ##### Check inputs ###########################################################

  check_clusters_per_time_period(clusters_per_time_period)
  check_labels(labels)
  check_names(row_names, col_names, clusters_per_time_period)

  ##### Build design matrix ####################################################

  C                   <- sum(clusters_per_time_period)
  Ti                  <- length(clusters_per_time_period)
  X                   <- matrix(labels[1], C, Ti)
  c                   <- 1L
  for (j in 1:Ti) {
    if (clusters_per_time_period[j] > 0) {
      temp            <- c + clusters_per_time_period[j] - 1L
      X[c:temp, j:Ti] <- labels[2]
      c               <- temp + 1L
    }
  }
  rownames(X)         <- row_names
  colnames(X)         <- col_names

  ##### Output #################################################################

  X

}