#' Read an ecrypted message and decrypt it
#'
#' @param file The file containing the encrypted message
#' @param plain_file The file for the decrypted message (as PNG)
#' @param key_bits The bits to decrypt the message
#'
#' @return A matrix with the plain text, decrypted message pixel values
#' @export
#'
#' @examples
#' # decrypted_image = read_encrypted_image(file, plain_file = "decrypted.png", key_bits)
read_encrypted_image = function(file, plain_file = "decrypted.png", key_bits){
  image = png::readPNG(file)
  image = image * 255
  plain_text = image
  # xor the last two bits of each element
  for(r in 1:nrow(image)){
    for(c in 1:ncol(image)){
      image_bits = intToBits(image[r,c])
      xor_result = xor(image_bits[1],key_bits[r,c])
      plain_text[r,c] = as.integer(xor_result)
    }
  }
  png::writePNG(
    plain_text,
    target = plain_file
  )
  return(plain_text)
}
