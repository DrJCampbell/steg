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
read_encrypted_image = function(file, plain_file = "decrypted.png", key){

  image = png::readPNG(file)
  image = image * 255

  plain_text = matrix(NA, nrow=nrow(image), ncol=ncol(image))

  # create key bits for xoring with the message bit
  key_bits = create_key_bits(key = key, image = image)

  # create a channel selection for hoping between RGB channels
  if(!is.na(dim(image)[3])){
    if(dim(image)[3] > 2){
      channel_select = create_channel_select(image, key)
    }
  }

  # xor the last two bits of each element
  for(r in 1:nrow(image)){
    for(c in 1:ncol(image)){

      if(is.na(dim(image)[3])){
        # image is single-channel grey scale
        image_bits = intToBits(image[r,c])
      } else {
        image_bits = intToBits(image[r,c, channel_select[r,c]])
      }

      xor_result = xor(image_bits[1],key_bits[r,c])
      plain_text[r,c] = as.integer(xor_result)
    }
  }
  plain_text = plain_text / 1
  png::writePNG(
    plain_text,
    target = plain_file
  )
  return(plain_text)
}
