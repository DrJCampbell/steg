#' Examine an image at a specific level of significant bits
#'
#' @param file The PNG image file to examine
#' @param layer The layer (bit significance) of the PNG image to view
#'
#' @return Object of class image
#' @export show_image_layer
#'
#' @examples
#' # show_image_layer(file="John_Tukey.png", layer = 1)
show_image_layer = function(file, layer=1, channel=1){
  if(length(layer) != 1){
    stop("layer needs to be a single value in the range 1:8")
  }
  if(!(layer %in% 1:8)){
    stop("layer need to be in the range of 1 to 8")
  }

  #x = matrix(1:9, nrow=3)
  x = png::readPNG(file)
  x = x * 255
  ##x = x[layer]

  image=matrix(NA, nrow=nrow(x), ncol=ncol(x))

  for(r in 1:nrow(x)){
    for(c in 1:ncol(x)){
      x_bits = intToBits(x[r,c, channel])
      image[r,c] = as.integer(x_bits[layer])
    }
  }

  flipped = t(image)
  flipped = flipped[,ncol(flipped):1]
  graphics::par(mai = c(0,0,0,0))
  graphics::image(flipped, col=c("#000000", "#FFFFFF"))
}
