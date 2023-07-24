#' Reveal a message hidden in a photo using the key
#'
#' @param key The text to be used as a key to seed the hash
#' @param crypto_image The file name of the PNG file with the hidden message
#'
#' @return NULL
#' @export reveal_message
#'
#' @examples
#' # reveal_message(
#' #   key="UseABetterKeyThanThis",
#' #   crypto_image="crypto_image.png"
#' #   )
reveal_message = function(key, crypto_image){

  # Here we are only reading the crypto iamge to get the number of pixels in the image
  image = read_original_image(file = crypto_image)

  # Build the crypto bits from the key
  key_bits = create_key_bits(key = key, image = image)

  #
  plain_text = read_encrypted_image(crypto_image, key_bits = key_bits)

  # Plot the decrypted data to view the message
  plot_decrypted_image(plain_text)

  return(NULL)
}
