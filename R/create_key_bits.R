#' Create ley bits from CBC hashes of a key
#'
#' @param key A text string to use as a seed for the key
#' @param image The image that the key will be used with
#'
#' @return A matrix of binary values [0,1] with dimensions equal to the image data
#' @export
#'
#' @examples
#' # Assuming we have already created photo using `read_original_image()`
#' # key_bits = create_key_bits(key = "This_is_the_key", image = photo)
create_key_bits = function(key, image){
  required_length = length(as.numeric(image))
  # Get 512 bits per hash
  number_extra_hashes = (required_length %/% 512)
  digest = digest::digest(key, "sha256", serialize = FALSE)
  for(i in 1:number_extra_hashes){
    digest = paste0(digest, digest::digest(digest, "sha256", serialize = FALSE))
  }
  digest_bits = rawToBits(charToRaw(digest))[1:required_length]
  key_matrix = matrix(digest_bits[1:required_length], nrow = nrow(image))
  return(key_matrix)
}
