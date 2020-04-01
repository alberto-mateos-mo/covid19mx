#' Function to split strings with the option to not remove the split
#' 
#' @param x character vector
#' @param split character vector to use for splitting
#' @param type how to split, 'remove' to the split, 'before' to keep the split to the left and 'after' to keep the split to the right. Defaults to 'remove'.
#' @param perl logical. Should Perl-compatible regexps be used?
#' @param ... other arguments to be passed to strsplit
#' @export
#' 

strsplit2 <- function(x,
                      split,
                      type = "remove",
                      perl = FALSE,
                      ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}
