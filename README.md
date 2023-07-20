
<!-- README.md is generated from README.Rmd. Please edit that file -->

# steg

<!-- badges: start -->
<!-- badges: end -->

The package `steg` provides cryptographic tools for hiding messages
inside photos.

It was developed as a fun project and **should not be used where serious
cryptography is required**.

## Installation

You can install the development version of steg from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DrJCampbell/steg")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r

library(steg)

# We include an example file 'John_Tukey.png' in the packages extdata folder
example_image_file = system.file("extdata", "John_Tukey.png", package = "steg")

# Read the example image photo into a matrix of bits
image = read_original_image(file = example_image_file)

# Generate a pseudorandom string of bits using the sha256 algorithm seeded with the key
# The hashes are cypher-block-chained to produce as many bits as there are pixels in the image
key_bits = create_key_bits(key = "Use_a_really_good_key_here", image = image)

# Create an image containing the message to be hidden
message = create_message_image(
  file = "message.png",
  message = "You can replace this message with up to 122 characters that will be written in a png file",
  image = image
  )

# Hide the message in the image and write out as a png file
crypto = write_encrypted_image("crypto.png", message, image, key_bits = key_bits)


# The crypto.png file could be sent to a third party. If they also know the key, they
# will be able to decrypt the message
plain_text = read_encrypted_image("crypto.png", key_bits = key_bits)

# Plot the decrypted data to view the message
plot_decrypted_image(plain_text)
```
