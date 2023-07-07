#' Read a photo into matrix
#'
#' @param file The photo to be read
#'
#' @return Image pixel values as a matrix
#' @export
#'
#' @examples
#' # photo = read_original_image(file = "John_Tukey.png")
read_original_image = function(file = "John_Tukey.png"){
  x = png::readPNG(file)
  x = x * 255
  return(x)
}
