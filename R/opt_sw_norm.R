#' @export
opt_sw_norm <- function(C = 20, Ti = 10, m = 10, rho0 = 0.1, r = 1,
                        extreme_seq = TRUE, time_effect = "discrete", init_w) {

  ##### Check inputs ###########################################################

  C  <- check_integer_range(C, "C", c(1, Inf), 1)
  Ti <- check_integer_range(Ti, "Ti", c(2, Inf), 1)
  check_real_range_strict(m, "m", c(0, Inf), 1)
  check_real_range(rho0, "rho0", c(0, 1), 1)
  check_real_range_lstrict(r, "r", c(0, 1), 1)
  check_logical(extreme_seq, "extreme_seq")
  check_belong(time_effect, "time_effect",c("discrete", "linear", "quadratic",
                                             "cubic", "quartic", "quintic"))

  ##### Preliminaries ##########################################################

  beq                       <- 1
  num_seqs                  <- Ti - 1 + 2*extreme_seq
  len_w                     <- ceiling(num_seqs/2)
  if (Ti%%2 == 0) {
    Aeq                     <- matrix(c(rep(2, len_w - 1), 1), 1, len_w)
    ub                      <- c(rep(0.5, len_w - 1), 1)
  } else {
    Aeq                     <- matrix(2, 1, len_w)
    ub                      <- rep(0.5, len_w)
  }
  Ti_min_1                  <- Ti - 1
  seq_Ti_min_1              <- 1:Ti_min_1
  X_seqs                    <- matrix(0, Ti_min_1, Ti)
  X_seqs[upper.tri(X_seqs)] <- 1
  X_seqs                    <- X_seqs[rev(seq_Ti_min_1), ]
  if (extreme_seq) {
    X_seqs                  <- rbind(0, X_seqs, 1)
  }

  ##### Find the optimal weights ###############################################

  if (missing(init_w)) {
    init_w                <- rep(1/len_w, len_w)
  }
  optimal_solution_exact  <- pracma::fmincon(x0            = init_w,
                                             fn            =
                                               opt_sw_norm_internal,
                                             gr            = NULL,
                                             C             = C,
                                             Ti            = Ti,
                                             m             = m,
                                             rho0          = rho0,
                                             r             = r,
                                             extreme_seq   = extreme_seq,
                                             time_effect   = time_effect,
                                             sigma         = 1,
                                             method        = "SQP",
                                             A             = NULL,
                                             b             = NULL,
                                             Aeq           = Aeq,
                                             beq           = beq,
                                             lb            = rep(0, len_w),
                                             ub            = ub,
                                             hin           = NULL,
                                             heq           = NULL,
                                             tol           = 1e-11,
                                             maxfeval      = 1e6,
                                             maxiter       = 1e6)
  if (Ti%%2 == 0) {
    optimal_weights_exact <- c(optimal_solution_exact$par,
                               rev(optimal_solution_exact$par[-len_w]))
  } else {
    optimal_weights_exact <- c(optimal_solution_exact$par,
                               rev(optimal_solution_exact$par))
  }
  rownames(X_seqs)        <-
    paste0("w[", rev(0:(num_seqs - 1)), "] = ", round(optimal_weights_exact, 3))

  ##### Find optimal exact design amongst near candidates ######################

  optimal_num_exact                <- C*optimal_weights_exact
  optimal_num_rounded              <- round(optimal_num_exact)
  fact                             <- (num_seqs <= 10)
  poss_allocations                 <-
    try(iterpc::getall(iterpc::iterpc(ifelse(fact, 3, 2), num_seqs,
                                      ordered = TRUE, replace = TRUE)),
        silent = TRUE)
  if (!("try-error" %in% class(poss_allocations))) {
    for (i in 1:num_seqs) {
      if (optimal_num_rounded[i] == 0) {
        poss_allocations[, i]      <- (0:(1 + fact))[poss_allocations[, i]]
      } else if (optimal_num_rounded[i] == C) {
        poss_allocations[, i]      <-
          ((C - (1 + fact)):C)[poss_allocations[, i]]
      } else {
        if (num_seqs <= 10) {
          poss_allocations[, i]    <-
            ((optimal_num_rounded[i] - 1):
               (optimal_num_rounded[i] + 1))[poss_allocations[, i]]
        } else {
          poss_allocations[, i]    <-
            c(floor(optimal_num_exact[i]),
              ceiling(optimal_num_exact[i]))[poss_allocations[, i]]
        }
      }
    }
    poss_allocations               <-
      poss_allocations[Rfast::rowsums(poss_allocations) == C, ]
    num_poss_allocations           <- nrow(poss_allocations)
    poss_var                       <- numeric(num_poss_allocations)
    for (i in 1:num_poss_allocations) {
      poss_var[i]                  <-
        opt_sw_norm_internal(poss_allocations[i, ]/C, C, Ti, m, rho0, r,
                             extreme_seq, time_effect)
    }
    optimal_num_rounded            <-
      poss_allocations[which(poss_var == min(poss_var))[1], ]
    optimal_weights_rounded        <- optimal_num_rounded/C
    optimal_design_rounded         <- matrix(0, C, Ti)
    counter                        <- 1
    for (i in 1:num_seqs) {
      if (optimal_num_rounded[i] > 0) {
        optimal_design_rounded[counter:(counter +
                                          optimal_num_rounded[i] - 1), ] <-
          matrix(X_seqs[i, ], optimal_num_rounded[i], Ti, byrow = TRUE)
        counter                    <- counter + optimal_num_rounded[i]
      }
    }
  } else {
    optimal_design_rounded         <- optimal_num_rounded <-
      optimal_weights_rounded <- NULL
  }
  names(optimal_num_exact)         <- paste0("Cw[", rev(0:(num_seqs - 1)), "]")
  names(optimal_num_rounded)       <- paste0("[Cw[", rev(0:(num_seqs - 1)),
                                             "]]")
  names(optimal_weights_exact)     <- paste0("w[", rev(0:(num_seqs - 1)), "]")
  names(optimal_weights_rounded)   <- paste0("[Cw[", rev(0:(num_seqs - 1)),
                                             "]]/C")
  optimal_design_rounded           <- optimal_design_rounded[C:1, ]
  rownames(optimal_design_rounded) <- paste0("c = ", 1:C)

  ##### Output #################################################################

  output <- list(optimal_design_exact    = X_seqs[num_seqs:1, ],
                 optimal_design_rounded  = optimal_design_rounded,
                 optimal_num_exact       = rev(optimal_num_exact),
                 optimal_num_rounded     = rev(optimal_num_rounded),
                 optimal_weights_exact   = rev(optimal_weights_exact),
                 optimal_weights_rounded = rev(optimal_weights_rounded),
                 inputs                  = list(C           = C,
                                                Ti          = Ti,
                                                m           = m,
                                                rho0        = rho0,
                                                r           = r,
                                                extreme_seq = extreme_seq,
                                                time_effect = time_effect))
  class(output) <- c("swcrt_opt_sw_norm", class(output))
  output

}