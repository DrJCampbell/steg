#' Plot a decrypted message
#'
#' @param plain_text The matrix of decrypted bits to display
#'
#' @return NULL
#' @export
#'
#' @examples
#' plain_text = matrix(sample(0:1, size=1000, replace=TRUE), ncol=10)
#' plot_decrypted_image(plain_text)
plot_decrypted_image = function(plain_text){
  flipped = t(plain_text)
  flipped = flipped[,ncol(flipped):1]
  graphics::par(mai = c(0,0,0,0))
  graphics::image(flipped, col=c("#000000", "#FFFFFF"))
}
