build_row_names <- function(plot_design, cp_contents, row_names, combine_rows) {
  nrow_plot_design                       <- nrow(plot_design)
  if (row_names) {
    if (combine_rows) {
      row_names_plot_design              <- character(nrow_plot_design)
      row_names_cp_contents              <- rownames(cp_contents)
      for (s in 1:nrow_plot_design) {
        matching_s                       <-
          sort(which(apply(cp_contents, 1,
                           function(x) {
                             all(as.vector(x) == as.vector(plot_design[s, ]))
                           })))
        len_matching_s                   <- length(matching_s)
        row_names_plot_design[s]         <- row_names_cp_contents[matching_s[1]]
        if (len_matching_s > 1) {
          for (m in 2:len_matching_s) {
            row_names_plot_design[s]     <-
              paste0(row_names_plot_design[s], ", ",
                     row_names_cp_contents[matching_s[m]])
          }
        }
      }
    } else {
      row_names_plot_design              <- rownames(plot_design)
    }
  } else {
    if (combine_rows) {
      row_names_plot_design              <- character(nrow_plot_design)
      for (s in 1:nrow_plot_design) {
        matching_s                       <-
          sort(which(apply(cp_contents, 1,
                           function(x) {
                             all(as.vector(x) == as.vector(plot_design[s, ]))
                           })))
        len_matching_s                   <- length(matching_s)
        if (len_matching_s == 1) {
          row_names_plot_design[s]       <- matching_s
        } else {
          row_names_plot_design[s]       <-
            paste0(matching_s[1],
                   ifelse(matching_s[2] == matching_s[1] + 1, "-", ", "),
                   matching_s[2])
          if (len_matching_s > 2) {
            for (m in 3:len_matching_s) {
              if (matching_s[m] == matching_s[m - 1] + 1) {
                row_names_plot_design[s] <-
                  paste0(strsplit(row_names_plot_design[s], "-")[[1]][-2], "-",
                         matching_s[m])
              } else {
                row_names_plot_design[s] <- paste0(row_names_plot_design[s],
                                                   ", ", matching_s[m])
              }
            }
          }
        }
      }
    } else {
      row_names_plot_design              <- 1:nrow_plot_design
    }
  }
  row_names_plot_design
}