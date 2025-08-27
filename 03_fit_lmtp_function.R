
fit_and_estimate_lmtp_effect <- function(dat, t_max = 3L) {

  policy <- function(data, trt) {
    (data[[trt]] - 1) * (data[[trt]] - 1 >= 0) + data[[trt]] * (data[[trt]] - 1 < 0)
  }

  A <- paste0("A_", seq(1, t_max-1))

  L <- lapply(1:(t_max-1), function(t) {
    paste0(c("L1_", "L2_"), t)
  })

  W <- c(paste0(c("L1_", "L2_"), 0), "V_0")

  lmtp_fit <- lmtp::lmtp_tmle(
    dat,
    trt = A,
    outcome = paste0("Y_", t_max),
    baseline = W,
    time_vary = L,
    shift = policy,
    mtp = TRUE,
    folds = 10)

  return(lmtp_fit)
}
