create_channel_select = function(image, key){
  # Mash the key bits to produce a seed for the PRNG
  key_chars = strsplit(key, split="")[[1]]
  # seed starts out as bits for a space character
  seed = rawToBits(charToRaw(" "))
  for(char in key_chars){
    # recursively xor the seed bits with bits for each character in the key
    seed = xor(seed, rawToBits(charToRaw(char)))
  }
  seed = as.integer(as.double(paste0(seed, collapse = "")) %% 2^16)
  set.seed(seed)
  channel_select = matrix(
    sample(
      c(1,2,3),
      size=(dim(image)[1] * dim(image)[2]),
      replace = TRUE
    ),
    nrow=nrow(image)
  )
  return(channel_select)
}
