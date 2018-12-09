
filecontent <- read_file(pathoffirstfile)
file.remove(pathoffirstfile)

library(sodium)
library(RCurl)
library(stringi)
library(readr)

pathoffirstfile <- "C:/Users/user/Desktop/firstfile.txt"

encrypt <- function(filecontenti, filepathi, keyi, noncei){
  sfilecontent <- serialize(filecontenti, NULL)
  filecontenti <- data_encrypt(sfilecontent, keyi, noncei)
  filecontenti <- as.vector(filecontenti)
  writeBin(filecontenti, filepathi)
}


key <- hash(charToRaw(toString(as.character("ABCDEFGHIJKLMNOPQRSTUVW"), width = NULL)))

set.seed(1234)
nonce <- random(24)
randomness <-  stri_rand_strings(n=1 , length = 24, pattern = "[A-Za-z0-9]")
nonce <- charToRaw(randomness)
encrypt(filecontent, pathoffirstfile, key, nonce)


paste("http://13.232.35.8:8080/known?","id=1","&count=7","&nonce=122",sep = "")

url = "http://13.232.35.8:8080/known?id=1&count=7&nonce=122"
xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
stringi::stri_extract_all_regex(xdata, '(?<=").*?(?=")')