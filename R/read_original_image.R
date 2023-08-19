#' Read a photo into matrix
#'
#' @param file The photo to be read
#' @param bitdepth The bitdepth of the photo as obtained from get_image_bitdepth
#'
#' @return Image pixel values as a matrix
#' @export read_original_image
#'
#' @examples
#' # photo = read_original_image(file = "John_Tukey.png")
read_original_image = function(file, bitdepth=255){
  x = png::readPNG(file, info = FALSE)
  x = x * bitdepth
  # If image is 16 bit, convert to 8 bit...
  # ((65535+1) / 2^8) - 1 == 255
  # ((65279+1) / 2^8) - 1 == 254
  #if(bitdepth == 65535){
  #  x = ((x + 1) / 256) - 1
  #}
  dims = dim(x)
  if(bitdepth == 255){
    image = x
  } else {
    if(length(dims) == 2){
      image = array(dim=c(dims[1],dims[2]))
      for(r in 1:dims[1]){
        for(c in 1:dims[2]){
          bits = intToBits(x[r,c])
          image[r,c] = packBits(c(bits[9:32], bits[25:32]), type = "integer")
        }
      }
    } else {
      image = array(dim=c(dims[1], dims[2], dims[3]))
      for(p in 1:dims[3]){
        for(r in 1:dims[1]){
          for(c in 1:dims[2]){

            bits = intToBits(x[r,c,p])
            image[r,c,p] = packBits(c(bits[9:32], bits[25:32]), type = "integer")
          }
        }
      }
    }
  }
  return(image)
}
