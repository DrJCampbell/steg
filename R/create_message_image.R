#' Create an image containing text to be encrypted
#'
#' @param file A file for the PNG to be written to
#' @param message The text to inlude in the image
#' @param photo The matrix containing the original image bits
#'
#' @return A matrix containing pixel data for the message image
#' @export
#'
#' @examples
#' photo <- matrix(0, ncol=500, nrow=500)
#' message_bits <- create_message_image(file = "message.png", message = "This is the\nmessage", photo)
create_message_image = function(file = "message.png", message = "This is the\nmessage", photo){

  # add new lines every... 14 chars? allow a maximum 112 chars
  message = gsub("(.{14})", "\\1\n", message, perl = T)

  grDevices::png(file, width = ncol(photo), height = nrow(photo))
  graphics::par(mai = c(0,0,0,0))
  plot(
    NULL, NULL,
    xlim = c(0,10),
    ylim = c(0, 10),
    bty = "n",
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  graphics::text(
    0, 4,
    message,
    pos = 4,
    cex = 3
  )
  grDevices::dev.off()
  x = png::readPNG(file)
  x = x * 255
  x = x[,,1]
  return(x)
}
