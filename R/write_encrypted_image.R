#' Encrypt and message in a photo
#'
#' @param file The file to write the encrypted photo to
#' @param message The message matrix from create_message_image()
#' @param image The photo to hide the message in, from read_original_image()
#' @param key The key for encryption
#'
#' @return A matrix with the encrypted message hidden in the image pixel values
#' @export
#'
#' @examples
#' # write_encrypted_image(file, message, image, key_bits)
write_encrypted_image = function(file, message, image, key){
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
  # Generate a pseudorandom string of bits using the sha256 algorithm seeded with the key
  # The hashes are cypher-block-chained to produce as many bits as there are pixels in the image
  key_bits = create_key_bits(key = key, image = image)
  # create a channel selection for hoping between RGB channels
  if(!is.na(dim(image)[3])){
    if(dim(image)[3] > 2){
      channel_select = create_channel_select(image, key)
    }
  }

  # Thoughts on flipping bits, integers or floats...
  # Alternative idea -> used this one
  # * when reading a 16 bit image
  # * do the conversion to 8 bits in read_original_image


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

      # Modification involves xoring the least significant bit of the
      # image with the top bit of the message

      image_bits[1] = xor(message_bits[1],key_bits[r,c])
      # convert the image_bits back to range 0:1
      # store the result in the last bit of
      # the image
      #modified_bit = packBits(image_bits, type = "integer") / bitdepth
      modified_bit = packBits(image_bits, type = "integer")
      if(is.na(dim(image)[3])){
        image[r,c] = modified_bit
      } else {
        image[r,c, channel_select[r,c]] = modified_bit
      }
    }
  }
  image = image / 255
  png::writePNG(
    image,
    target = file
  )
  return(image)
}
