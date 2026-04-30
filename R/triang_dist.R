#' Triangular Distribution
#'
#' @description Density, distribution function, quantile function and random
#' generation for the triangular distribution with parameters min, max and mode.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param min lower limit of the distribution (a).
#' @param max upper limit of the distribution (b).
#' @param mode peak of the distribution (c).
#'
#' @return `dtriang` gives the density, `ptriang` gives the
#' distribution function,
#' `qtriang` gives the quantile function, and `rtriang`
#' generates random deviates.
#' @name triangdist
NULL

check_params <- function(min, max, mode) {
  if (any(min > max, na.rm = TRUE)) {
    stop("min cannot be greater than max")
  }
  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("mode must be between min and max")
  }
}

#' @rdname triangdist
#' @export
dtriang <- function(x,
                    min = 0,
                    max = 1,
                    mode = 0.5) {
  check_params(min, max, mode)

  ifelse(x < min |
           x > max, 0, ifelse(x < mode, 2 * (x - min) /
                                ((max - min) * (mode - min)),
                              ifelse(x == mode, 2 /
                                       (max - min), 2 *
                                       (max - x) /
                                       ((max - min) *
                                           (max - mode)
                                       ))))
}

#' @rdname triangdist
#' @export
ptriang <- function(q,
                    min = 0,
                    max = 1,
                    mode = 0.5) {
  check_params(min, max, mode)

  ifelse(q <= min, 0, ifelse(q < mode, (q - min)^2 /
                               ((max - min) * (mode - min)),
                             ifelse(q < max, 1 - (max - q)^2
                                    / ((max - min) * (max - mode)
                                    ), 1)))
}

#' @rdname triangdist
#' @export
qtriang <- function(p,
                    min = 0,
                    max = 1,
                    mode = 0.5) {
  check_params(min, max, mode)
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop("p must be between 0 and 1")
  }

  fc <- (mode - min) / (max - min)

  ifelse(p < fc, min + sqrt(p * (max - min) * (mode - min)),
         max - sqrt((1 - p) * (max - min) * (max - mode)))
}

#' @rdname triangdist
#' @export
rtriang <- function(n,
                    min = 0,
                    max = 1,
                    mode = 0.5) {
  u <- stats::runif(n)
  qtriang(
    p = u,
    min = min,
    max = max,
    mode = mode
  )
}
