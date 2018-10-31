#' Color Scheme
#'
#' Function to adjust the transparency and define the color scheme within the visualization.
#'
#' @param colortype Color palette
#' @param alpha Transparency
#' @keywords pathway decision tree
#' @export
#' 
makeTransparent <- function(colortype, alpha) {
  ## Helper function to make colors transparent
  if (alpha < 0 | alpha > 1) stop("alpha must be between 0 and 1")
  alpha <- floor(255 * alpha)
  # color = rainbow_hcl()
  newColor <- col2rgb(col = unlist(list(colortype)), alpha = FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red = col[1], green = col[2], blue = col[3], alpha = alpha, maxColorValue = 255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha = alpha)
  return(newColor)
}
