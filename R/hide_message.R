#' Hide a message in a photo using a key
#'
#' @param image The PNG image file you want to hide the message in
#' @param key The text to be used as a key to seed the hash
#' @param message The plain-text message to hide in the photo
#' @param crypto_image The file name to write the photo with the hidden message to
#'
#' @return NULL
#' @export hide_message
#'
#' @examples
#' # hide_message(
#' #   image="John_Tukey.png",
#' #   key="UseABetterKeyThanThis",
#' #   message="Put your message here - up to 112 characters",
#' #   crypto_image="crypto_image.png"
#' #   )
hide_message = function(image, key, message, crypto_image){

  # Read the example image photo into a matrix of bits
  image = read_original_image(file = image)

  # Create an image containing the message to be hidden
  message = create_message_image(
    file = "message.png",
    message = message,
    image = image
  )

  # Hide the message in the image and write out as a png file
  crypto = write_encrypted_image(crypto_image, message, image, key = key)
  return(NULL)
}
