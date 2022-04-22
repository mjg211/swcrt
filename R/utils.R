build_row_names        <- function(plot_design, cp_contents, row_names,
                                   combine_rows) {
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
      row_names_plot_design              <- rownames(cp_contents)
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

information_cs_sw_norm <- function(C, Ti, m, sigma_e2, sigma_c2, U, V, W) {
  scaled_sigmae2 <- sigma_e2/m
  ((C*U - W)*scaled_sigmae2 + (U^2 + C*Ti*U - Ti*W - C*V)*sigma_c2)/
    (C*scaled_sigmae2*(scaled_sigmae2 + Ti*sigma_c2))
}

opt_sw_norm_internal   <- function(w, C, Ti, m, rho0, r, extreme_seq,
                                   time_effect, sigma = 1) {
  if (Ti%%2 == 0) {
    w_internal              <- c(w, rev(w[-length(w)]))
  } else {
    w_internal              <- c(w, rev(w))
  }
  sigma2                    <- sigma^2
  sigma_c2                  <- rho0*sigma2
  sigma_e2                  <- (1 - rho0)*sigma_c2/rho0
  zero_TiTi                 <- matrix(0, Ti, Ti)
  I_Ti                      <- diag(Ti)
  Ti_min_1                  <- Ti - 1
  seq_Ti_min_1              <- 1:Ti_min_1
  R                         <- toeplitz(c(1, r^(2*seq_Ti_min_1/Ti_min_1)))
  omega_denom               <- sigma_c2 + sigma_e2/m
  omega                     <- sigma_c2/omega_denom
  Vinv                      <-
    MASS::ginv(omega_denom*(omega*R + (1 - omega)*I_Ti))
  num_seqs                  <- Ti - 1 + 2*extreme_seq
  len_fixed                 <-
    dplyr::case_when(time_effect == "discrete" ~ Ti + 1,
                     time_effect == "linear" ~ 3,
                     time_effect == "quadratic" ~ 4,
                     time_effect == "cubic" ~ 5,
                     time_effect == "quartic" ~ 6,
                     time_effect == "quintic" ~ 7)
  info_mat                  <- array(0, c(len_fixed, len_fixed, num_seqs))
  if (time_effect == "discrete") {
    D_fact                  <- cbind(1, rbind(0, diag(Ti - 1)))
  } else {
    D_fact                  <- cbind(1, matrix(0, Ti, len_fixed - 2))
    time_pts                <- seq(-1, 1, length.out = Ti)
    for (i in 1:(len_fixed - 2)) {
      D_fact[, i + 1]       <- time_pts^i
    }
  }
  X_seqs                    <- zero_TiTi[-1, ]
  X_seqs[upper.tri(X_seqs)] <- 1
  X_seqs                    <- X_seqs[rev(seq_Ti_min_1), ]
  if (extreme_seq) {
    X_seqs                  <- rbind(0, X_seqs, 1)
  }
  for (i in 1:num_seqs) {
    design_mat              <- cbind(D_fact, X_seqs[i, ])
    info_mat[, , i]         <- w_internal[i]*t(design_mat)%*%Vinv%*%design_mat
  }
  MASS::ginv(C*rowSums(info_mat, dim = 2))[dim(info_mat)[1], dim(info_mat)[2]]
}
