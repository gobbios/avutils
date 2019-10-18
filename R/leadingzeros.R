#' add zeros to number as string for composing file names
#'
#' @param num numeric or character, the number
#' @param ndigits numeric, the total number of digits for the output
#' @param ext character, optional, a file extension to be added
#'
#' @return character
#' @export
#'
#' @examples
#' leadingzeros(3, 2)
#' leadingzeros(3, 5)
#' leadingzeros(333, 3)
#' leadingzeros(333, 6)
#' leadingzeros(333, 6, ".wav")
#' # produces an error
#' # leadingzeros(333, 2)
#'
#' # similar to
#' sprintf("%06.0f", 3)

leadingzeros <- function(num, ndigits, ext = NULL) {
  if (nchar(num) > ndigits) stop("number of characters in 'num' exceeds number of digits")
  x <- paste0(paste(rep(0, ndigits), collapse = ""), num)
  x <- substr(x, nchar(x) - ndigits + 1, nchar(x))
  if (!is.null(ext)) {
    x <- paste0(x, ext)
  }
  x
}
