#' Draw and export stepped-wedge cluster randomised trial design diagrams
#'
#' ...
#'
#' @param design ...
#' @param cp_contents ...
#' @param row_names ...
#' @param col_names ...
#' @param colours ...
#' @param combine_rows ...
#' @param merge_cols ...
#' @param xlab ...
#' @param ylab ...
#' @param cp_text_colour ...
#' @param key_interventions ...
#' @param key_time_periods ...
#' @param make ...
#' @param filename ...
#' @usage draw_sw(design         = design_sw(),
#'         cp_contents    = design,
#'         row_names      = FALSE,
#'         col_names      = FALSE,
#'         colours,
#'         combine_rows   = FALSE,
#'         merge_cols     = FALSE,
#'         xlab           = "Time period",
#'         ylab           = "Cluster",
#'         cp_text_colour = "black",
#'         key_interventions,
#'         key_time_periods,
#'         make           = "print",
#'         filename       = "swcrt")
#' @return A \code{\link{list}} containing the following elements:
#' \itemize{
#' \item A \code{\link{flextable}} in the slot \code{$table_design} containing
#' the constructed SW-CRT diagram.
#' \item A \code{\link{flextable}} in the slot \code{$table_interventions}
#' containing the constructed key summarising the various intervention states.
#' Will be \code{\link{NULL}} if input \code{key_interventions} is not
#' specified.
#' \item A \code{\link{flextable}} in the slot \code{$table_time_periods}
#' containing the constructed key summarising the various time periods.
#' Will be \code{\link{NULL}} if input \code{key_time_periods} is not
#' specified.
#' \item A \code{\link{list}} in the slot \code{$inputs} containing each of the
#' input variables.
#' }
#' @author Michael J Grayling (\email{michael.grayling@@newcastle.ac.uk})
#' @seealso \code{\link{design_sw}}
#' @examples
#' # The default is to draw a 'standard' SW-CRT design, with four clusters and
#' # five time periods
#' default_draw   <- draw_sw()
#' # Specify a more complex design, where all clusters begin in the intervention
#' # condition, there is an unequal number of clusters per wave, and there are
#' # two trailing extra time periods with all clusters in the intervention
#' # condition. Also modify the row and column names
#' complex_design <- design_sw(clusters_per_wave = c(1, 2, 3, 2, 0, 0),
#'                             row_names         = paste0("Cluster ", 1:8),
#'                             col_names         = paste0("Time period ", 1:6))
#' # Pass this to draw_sw(), producing .docx, .png, and .pptx files, merging
#' # columns, and combining rows
#' \dontrun{
#' complex_draw   <- draw_sw(complex_design, combine_rows = TRUE,
#'                           merge_cols = TRUE,
#'                           make = c("docx", "png", "pptx", "print"))
#' }
#' @export
draw_sw <- function(design         = design_sw(),
                    cp_contents    = design,
                    row_names      = FALSE,
                    col_names      = FALSE,
                    colours,
                    combine_rows   = FALSE,
                    merge_cols     = FALSE,
                    xlab           = "Time period",
                    ylab           = "Cluster",
                    cp_text_colour = "black",
                    key_interventions,
                    key_time_periods,
                    make           = "print",
                    filename       = "swcrt") {

  ##### Check inputs ###########################################################

  design              <- check_design(design)
  cp_contents         <- check_cp_contents(cp_contents, design)
  check_logical(row_names, "row_names")
  check_logical(col_names, "col_names")
  if (missing(colours)) {
    colours_input     <- NULL
  } else {
    colours_input     <- colours
  }
  colours             <- check_colours(colours, design)
  check_logical(combine_rows, "combine_rows")
  if (all(combine_rows, !all(design == cp_contents))) {
    warning("combine_rows = T, but this will have no effect as design != ",
            "cp_contents")
  }
  check_logical(merge_cols, "merge_cols")
  check_character(xlab, "xlab")
  check_character(ylab, "xlab")
  check_character(cp_text_colour, "cp_text_colour")
  check_keys(key_interventions, key_time_periods, design)
  if (missing(key_interventions)) {
    key_interventions <- NULL
  }
  if (missing(key_time_periods)) {
    key_time_periods  <- NULL
  }
  check_belong(make, "make", c("docx", "png", "pptx", "print"))
  check_character(filename, "filename")

  ##### Preliminaries ##########################################################

  # Determine the design to plot
  if (all(combine_rows, design == cp_contents)) {
    plot_design           <- dplyr::distinct(cp_contents)
    design_colours        <- plot_design
  } else {
    plot_design           <- cp_contents
    design_colours        <- design
  }
  # Store number of columns in this design so it doesn't need to be repeatedly
  # evaluated
  ncol_plot_design        <- ncol(plot_design)
  # Same for sequence 1,...,nrow(plot_design)
  seq_nrow_plot_design    <- 1:nrow(plot_design)
  # Determine row names
  row_names_plot_design   <- build_row_names(plot_design, cp_contents,
                                             row_names, combine_rows)
  # Determine column names
  if (col_names) {
    col_names_plot_design <- colnames(plot_design)
  } else {
    col_names_plot_design <- 1:ncol_plot_design
  }
  # Modify the design to include the column label and names
  plot_design             <- cbind(ylab, row_names_plot_design, plot_design)
  colnames(plot_design)   <- col_names_plot_design <- c(ylab, "i",
                                                        col_names_plot_design)

  ##### Build main design table ################################################

  # Convert to flextable to begin preparation for plotting
  table                                <- flextable::flextable(plot_design)
  # Add background colour to each of the cluster-periods
  names_colours                        <- names(colours)
  for (t in 3:(ncol_plot_design + 2)) {
    for (k in 1:length(colours)) {
      table                            <-
        flextable::bg(table, (design_colours[, t - 2] == names_colours[k]), t,
                      colours[k])
    }
  }
  # Auto-adjust the widths and heights of the cells
  table                                <- flextable::autofit(table)
  # Merge horizontally if this is desired
  if (merge_cols) {
    for (i in seq_nrow_plot_design) {
      j1                               <- 3L
      while (j1 < ncol_plot_design + 2L) {
        j2                             <- j1 + 1L
        check                          <- F
        while (all(plot_design[i, j1] == plot_design[i, j2],
                   j2 <= ncol_plot_design + 2L)) {
          j2                           <- j2 + 1L
          check                        <- T
        }
        if (check) {
          table                        <- flextable::merge_h_range(table, i,
                                                                   j1, j2 - 1)
        }
        j1                             <- j2
      }
    }
  }
  # Merge down first column so there's a single y label for all rows
  table                                <- flextable::merge_v(table, j = 1)
  # Centre the first column
  table                                <-
    flextable::align(table, j = 1, align = "center", part = "body")
  # Delete the column names
  values                               <- list()
  for (t in 1:(ncol_plot_design + 2)) {
    values[[col_names_plot_design[t]]] <- ""
  }
  table                                <-
    flextable::set_header_labels(table, values = values)
  # Modify the borders
  border_black                         <- officer::fp_border("black")
  border_transparent                   <- officer::fp_border("#00000000",
                                                             "none")
  table                                <- flextable::border_remove(table)
  table                                <-
    flextable::border_outer(table, border_black, "all")
  table                                <-
    flextable::border_inner_h(table, border_black, "all")
  table                                <-
    flextable::border_inner_v(table, border_black, "all")
  table                                <-
    flextable::border(table, j = 1, border = border_transparent)
  table                                <-
    flextable::border(table, j = 2, border.bottom = border_transparent,
                      border.left = border_transparent,
                      border.top = border_transparent)
  table                                <-
    flextable::border(table, border.top = border_transparent, part = "header")
  table                                <-
    flextable::border(table, 1, 1:2, border.bottom = border_transparent,
                      part = "header")
  table                                <-
    flextable::border(table, border.left = border_transparent,
                      border.right = border_transparent, part = "header")
  table                                <-
    flextable::border(table, j = 3, border.left  = border_black)
  # Add column names to footer
  table                                <-
    flextable::add_footer_row(table,
                              values    = c("", "",
                                            col_names_plot_design[-(1:2)]),
                              colwidths = rep(1L, ncol_plot_design + 2))
  # Add xlab
  table                                <-
    flextable::add_footer_row(table,
                              values    = c("", "", xlab),
                              colwidths = c(1L, 1L, ncol_plot_design),
                              top       = F)
  # Centre the footer
  table                                <-
    flextable::align(table, align = "center", part = "footer")
  # Center the body
  table                                <-
    flextable::align(table, align = "center", part = "body")
  # Standardise font size
  table                                <-
    flextable::fontsize(table, part = "all", size = 11)
  # Standardise the row heights
  heights                              <- dim(table)$heights
  table                                <-
    flextable::height_all(table, sum(heights)/length(heights))
  # Modify border in first column to avoid overlay issues
  table                                <-
    flextable::border(table, j = 1, border = border_black)
  table                                <-
    flextable::border(table, j = 1, border = border_transparent)
  # Change cluster-period text colour
  table                                <-
    flextable::color(table, i = seq_nrow_plot_design,
                     j = 3:(ncol_plot_design + 2), color = cp_text_colour)

  ##### Build keys #############################################################

  # Create the key for the interventions
  if (!is.null(key_interventions)) {
    interventions         <-
      data.frame(Label            = names(key_interventions),
                 Description      = key_interventions,
                 stringsAsFactors = F)
    table_interventions   <- flextable::flextable(interventions)
    # Add background colour to each of the rows
    for (k in 1:length(colours)) {
      table_interventions <-
        flextable::bg(table_interventions,
                      which(interventions[, 1] == names_colours[k]), 1:2,
                      colours[k])
    }
    # All a black border all around
    table_interventions   <- flextable::border(table_interventions,
                                               border = border_black,
                                               part   = "all")
    # Auto-adjust the widths and heights of the cells
    table_interventions   <- flextable::autofit(table_interventions)
    # Align left
    table_interventions   <- flextable::align(table_interventions, part = "all")
  } else {
    table_interventions   <- NULL
  }
  # Create the key for the time periods
  if (!is.null(key_time_periods)) {
    time_periods          <-
      data.frame(`Time period`    = col_names_plot_design[-(1:2)],
                 `Calendar time`  = key_time_periods,
                 stringsAsFactors = F)
    table_time_periods    <- flextable::flextable(time_periods)
    # All a black border all around
    table_time_periods    <- flextable::border(table_time_periods,
                                               border = border_black,
                                               part   = "all")
    # Change the header labels
    table_time_periods    <-
      flextable::set_header_labels(table_time_periods,
                                   Time.period   = "Time period",
                                   Calendar.time = "Calendar time")
    # Auto-adjust the widths and heights of the cells
    table_time_periods    <- flextable::autofit(table_time_periods)
    # Align left
    table_time_periods    <- flextable::align(table_time_periods, part = "all")
  } else {
    table_time_periods    <- NULL
  }

  ##### Write files and output #################################################

  # Produce .docx file if desired
  if ("docx" %in% make) {
    docx   <- officer::read_docx()
    docx   <- flextable::body_add_flextable(docx, table)
    if (!is.null(key_interventions)) {
      docx <- officer::body_end_section_continuous(docx)
      docx <- flextable::body_add_flextable(docx, table_interventions)
    }
    if (!is.null(key_time_periods)) {
      docx <- officer::body_end_section_continuous(docx)
      docx <- flextable::body_add_flextable(docx, table_time_periods)
    }
    print(docx, paste0(filename, ".docx"))
  }
  # Produce .pptx file if desired
  if ("pptx" %in% make) {
    pptx   <- officer::read_pptx()
    pptx   <- officer::add_slide(pptx, layout = "Title and Content",
                                 master = "Office Theme")
    pptx   <- officer::ph_with(pptx, table,
                               officer::ph_location_type(type = "body"))
    if (!is.null(key_interventions)) {
      pptx <- officer::add_slide(pptx, layout = "Title and Content",
                                 master = "Office Theme")
      pptx <- officer::ph_with(pptx, table_interventions,
                               officer::ph_location_type(type = "body"))
    }
    if (!is.null(key_time_periods)) {
      pptx <- officer::add_slide(pptx, layout = "Title and Content",
                                 master = "Office Theme")
      pptx <- officer::ph_with(pptx, table_time_periods,
                               officer::ph_location_type(type = "body"))
    }
    print(pptx, paste0(filename, ".pptx"))
  }
  # Produce .png file if desired
  if ("png" %in% make) {
    flextable::save_as_image(table, paste0(filename, ".png"))
    if (!is.null(key_interventions)) {
      flextable::save_as_image(table_interventions,
                               paste0(filename, "_key_interventions.png"))
    }
    if (!is.null(key_time_periods)) {
      flextable::save_as_image(table_time_periods,
                               paste0(filename, "_key_time_periods.png"))
    }
  }
  # Print table if desired
  if ("print" %in% make) {
    table
  }
  # Output table and inputs
  list(table_design        = table,
       table_interventions = table_interventions,
       table_time_periods  = table_time_periods,
       inputs              = list(design            = design,
                                  cp_contents       = cp_contents,
                                  row_names         = row_names,
                                  col_names         = col_names,
                                  colours           = colours_input,
                                  combine_rows      = combine_rows,
                                  merge_cols        = merge_cols,
                                  xlab              = xlab,
                                  ylab              = ylab,
                                  cp_text_colour    = cp_text_colour,
                                  key_interventions = key_interventions,
                                  key_time_periods  = key_time_periods,
                                  make              = make,
                                  filename          = filename))

}