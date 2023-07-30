#' Encrypt and message in a photo
#'
#' @param file The file to write the encrypted photo to
#' @param message The message matrix from create_message_image()
#' @param image The photo to hide the message in, from read_original_image()
#' @param key_bits The key for encryption
#'
#' @return A matrix with the encrypted message hidden in the image pixel values
#' @export
#'
#' @examples
#' # write_encrypted_image(file, message, image, key_bits)
write_encrypted_image = function(file, message, image, key_bits){
  # for each element in the message and image matrices,
  # get the value as bits. Consider the least two sig
  # bits and xor them. Store the result in the least
  # sig bit. To reverse this, xor the last two bits and
  # store the result * 255 in a decrypted matrix
  if(!all(dim(message)[1] == dim(image)[1] & dim(message)[2] == dim(image)[2])){
    stop(
      paste0(
        "Image matricies need to be the same size. image size is ",
        dim(image)[1],
        " by ",
        dim(image)[2],
        ". Message is ",
        dim(message)[1],
        " by ",
        dim(message)[2],
        "."
      )
    )
  }
  # modify the
  if(!is.na(dim(image)[3])){
    channel_select = matrix(
      rep(
        c(1,2,3),
        times=(length(as.numeric(image)) / dim(image)[3])
      )[1:(dim(image)[1] * dim(image)[2])],
      nrow=nrow(image)
    )
  }
  for(r in 1:nrow(image)){
    for(c in 1:ncol(image)){
      # convert values to bits
      message_bits = intToBits(message[r,c])
      # check if single (bw) channel or three (rgb)
      if(is.na(dim(image)[3])){
        # image is single-channel grey scale
        image_bits = intToBits(image[r,c])
      } else {
        image_bits = intToBits(image[r,c, channel_select[r,c]])
      }


      # xor the last bit of the message with
      # the last bit of the image
      image_bits[1] = xor(message_bits[1],key_bits[r,c])
      # convert the image_bits back to range 0:1
      # store the result in the last bit of
      # the image
      modified_bit = packBits(image_bits, type = "integer") / 255
      if(is.na(dim(image)[3])){
        image[r,c] = modified_bit
      } else {
        image[r,c, channel_select[r,c]] = modified_bit
      }
    }
  }
  png::writePNG(
    image,
    target = file
  )
  return(image)
}
