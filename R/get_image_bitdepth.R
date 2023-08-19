#' Read a photo at get it's bit depth
#'
#' @param file The photo to be read
#'
#' @return The bit depth of the photo
#' @export get_image_bitdepth
#'
#' @examples
#' # bitdepth = get_image_bitdepth(file = "John_Tukey.png")
get_image_bitdepth = function(file){
  x = png::readPNG(file, info = TRUE)
  bit_depth = (2 ^ attr(x,"info")$bit.depth) - 1

  return(bit_depth)
}
