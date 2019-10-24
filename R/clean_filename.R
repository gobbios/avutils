#' replace special characters with underscore
#'
#' produce file names that can be handled by the divime tools
#'
#' @param x character, typically a file name
#'
#' @return character, the file name with special characters replaced
#' @export
#'
#' @examples
#' filename <- "a file (final).wav"
#' clean_filename(filename)

clean_filename <- function(x) {
  xchars <- c(" " , "(",  ")")
  for (i in xchars) {
    x <- gsub(pattern = i, replacement = "_", x = x, fixed = TRUE)
  }
  x
}
