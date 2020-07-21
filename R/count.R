#' Counting functions

#' Plethora of function to extract features of a text.
#' 
#' @import textfeatures
#' @noRd

#' @export
n_words <- textfeatures:::n_words

#' @export
n_digits <- textfeatures:::n_digits

#' @export
n_nonasciis <- textfeatures:::n_nonasciis

#' @export
n_hashtags <- textfeatures:::n_hashtags

#' @export
n_mentions <- textfeatures:::n_mentions

#' @export
n_commas <- textfeatures:::n_commas

#' @export
n_periods <- textfeatures:::n_periods

#' @export
n_exclaims <- textfeatures:::n_exclaims

#' @export
n_newlines <- function(x) {
  na <- is.na(x)
  if (all(na))
    return(0)
  m <- gregexpr("\n", x)
  x <- vapply(m, function(x) sum(x > 0, na.rm = TRUE), FUN.VALUE = integer(1))
  x[na] <- NA_integer_
  x
}

#' @export
n_caps <- textfeatures:::n_caps

#' @export
n_lowers <- textfeatures:::n_lowers

#' @export
n_urls <- textfeatures:::n_urls

#' @export
n_puncts <- textfeatures:::n_puncts