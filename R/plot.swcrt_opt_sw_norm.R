#' @method plot swcrt_opt_sw_norm
#' @export
plot.swcrt_opt_sw_norm <- function(x = opt_sw_norm(), output = FALSE,
                                   print_plots = TRUE, summary = FALSE, ...) {

  ##### Check input variables ##################################################

  #check_swcrt_opt_sw_norm(x, name = "x")
  check_logical(output, "output")
  check_logical(print_plots, "print_plots")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_plot_swcrt_opt_sw_norm(x)
    message("")
  }

  ##### Perform main computations ##############################################

  if (all(summary, output)) {
    message("  Beginning production of plots..")
  }
  plots                <- list()
  C                    <- x$inputs$C
  Ti                   <- x$inputs$Ti
  df_rounded           <-
    tibble::tibble(Cluster        = rep(1:C, each = Ti),
                   `Time period`  = rep(1:Ti, C),
                   condition      =
                     factor(as.vector(t(x$optimal_design_rounded))))
  plots$rounded        <-
    ggplot2::ggplot(df_rounded,
                    ggplot2::aes(`Time period`, rev(Cluster),
                                 fill = condition)) +
    ggplot2::geom_tile(colour = "black", size = 0.5) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = seq(1, Ti, 1)) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                breaks = seq(1, C, 1)) +
    ggplot2::theme_bw() + ggplot2::scale_fill_viridis_d() +
    ggplot2::coord_equal() +
    ggplot2::theme(legend.position = "none",
                   axis.ticks.x    = ggplot2::element_blank(),
                   axis.ticks.y    = ggplot2::element_blank(),
                   axis.text.x     = ggplot2::element_text(colour = "black"),
                   axis.text.y     = ggplot2::element_text(colour = "black")) +
    ggplot2::ylab("Cluster")
  num_seqs             <- nrow(x$optimal_design_exact)
  weight_label         <- paste0("italic(w)[", 1:num_seqs, "] == ",
                                 round(x$optimal_weights_exact, 3))
  df_exact             <-
    tibble::tibble(sequence       = factor(rep(1:num_seqs, each = Ti)),
                   `Time period`  = rep(1:Ti, num_seqs),
                   weight         = rep(x$optimal_weights_exact, each = Ti),
                   condition      =
                     factor(as.vector(t(x$optimal_design_exact))),
                   `weight label` = factor(rep(weight_label, each = Ti),
                                           weight_label))
  plots$exact_equal    <-
    ggplot2::ggplot(df_exact,
                    ggplot2::aes(`Time period`, rev(`weight label`),
                                 fill = condition, group = sequence)) +
    ggplot2::geom_tile(colour = "black", size = 0.5) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = seq(1, Ti, 1)) +
    ggplot2::scale_y_discrete(expand = c(0, 0),
                              labels =
                                parse(text = levels(df_exact$`weight label`))) +
    ggplot2::theme_bw() + ggplot2::scale_fill_viridis_d() +
    ggplot2::coord_equal() +
    ggplot2::theme(legend.position = "none",
                   axis.ticks.x    = ggplot2::element_blank(),
                   axis.ticks.y    = ggplot2::element_blank(),
                   axis.text.x     = ggplot2::element_text(colour = "black"),
                   axis.text.y     = ggplot2::element_text(colour = "black")) +
    ggplot2::ylab("Sequence")
  plots$exact_weighted <-
    ggplot2::ggplot(df_exact,
                    ggplot2::aes(`Time period`, weight,
                                 fill = condition, group = rev(sequence))) +
    ggplot2::geom_bar(position = ggplot2::position_stack(reverse = TRUE),
                      stat = "identity", width = 1, colour = "black") +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = seq(1, Ti, 1)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_bw() + ggplot2::scale_fill_viridis_d() +
    ggplot2::theme(legend.position = "none",
                   axis.ticks.x    = ggplot2::element_blank(),
                   axis.text.x     = ggplot2::element_text(colour = "black"),
                   axis.text.y     = ggplot2::element_text(colour = "black")) +
    ggplot2::ylab("Cumulative weight by sequence")
  if (print_plots) {
    print(plots$rounded)
    print(plots$exact_equal)
    print(plots$exact_weighted)
  }
  if (all(summary, output)) {
    message("..completed production of plots.")
  }

  ##### Outputting #############################################################

  if (all(summary, output)) {
    message("  Preparing for outputting..")
    message("..outputting.")
  }
  if (output) {
    list(plots  = plots,
         inputs = list(x           = x,
                       output      = output,
                       print_plots = print_plots,
                       summary     = summary))
  }

}
