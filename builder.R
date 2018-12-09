#when building for first time run only the encrypt functions

#content of first file and building first file 

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file1.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/01.txt"

# filesourcepath <- paste(getwd(), "/file1.txt", sep = "") 
# filepath <-  paste(getwd(), "/01.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[A-Za-z0-9]")
nonce <- charToRaw(noncechar)
url <- "http://13.232.35.8:8080/known?id=1&count=1&nonce="
url <- paste(url, noncechar , sep = "" )
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)


filepathofencryptedfile <- "C:/Users/user/Desktop/Tally/ESA_Attributes/02.txt"
filepathofdecryptedfile <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file2.txt"

# filepathofencryptedfile <- paste(getwd(), "/02.txt", sep = "" ) 
# filepathofdecryptedfile <- paste(getwd(), "/file2.txt", sep = "") 


key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))
url <- "http://13.232.35.8:8080/known?id=1&count=2&nonce=a"
xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
noncechar <- stringi::stri_extract_all_regex(xdata, '(?<=").*?(?=")')[[1]]
nonce <- charToRaw(noncechar)

decryptandrun <- function(filepathofencryptedfile, filepathofdecryptedfile, key, nonce){
  dataencrypted <- read_file_raw(filepathofencryptedfile)
  dataafterdecrypt <-  unserialize(data_decrypt(dataencrypted, key = key, nonce = nonce))
  dataafterdecrypt <- as.vector(dataafterdecrypt)
  write(dataafterdecrypt, filepathofdecryptedfile)
  source(filepathofdecryptedfile)
}


decryptandrun(filepathofencryptedfile, filepathofdecryptedfile, key, nonce)


#content of second file and building second file 


library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file2.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/02.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

# filesourcepath <- paste(getwd(), "/file2.txt", sep = "") 
# filepath <-  paste(getwd(), "/02.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[A-Za-z0-9]")
nonce <- charToRaw(noncechar)
url <- "http://13.232.35.8:8080/known?id=1&count=2&nonce="
url <- paste(url, noncechar , sep = "" )
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)


filepathofencryptedfile <- "C:/Users/user/Desktop/Tally/ESA_Attributes/03.txt"
filepathofdecryptedfile <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file3.txt"

# filepathofencryptedfile <- paste(getwd(), "/03.txt", sep = "" ) 
# filepathofdecryptedfile <- paste(getwd(), "/file3.txt", sep = "") 

key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))
url <- "http://13.232.35.8:8080/known?id=1&count=3&nonce=a"
xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
noncechar <- stringi::stri_extract_all_regex(xdata, '(?<=").*?(?=")')[[1]]
nonce <- charToRaw(noncechar)

decryptandrun <- function(filepathofencryptedfile, filepathofdecryptedfile, key, nonce){
  dataencrypted <- read_file_raw(filepathofencryptedfile)
  dataafterdecrypt <-  unserialize(data_decrypt(dataencrypted, key = key, nonce = nonce))
  dataafterdecrypt <- as.vector(dataafterdecrypt)
  write(dataafterdecrypt, filepathofdecryptedfile)
  source(filepathofdecryptedfile)
}


decryptandrun(filepathofencryptedfile, filepathofdecryptedfile, key, nonce)

#content of third file and building third file 

library(sodium, lib.loc = .libPaths())
library(RCurl)
library(stringi)
library(readr)
library(envDocument)

filesourcepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file3.txt"
filepath <- "C:/Users/user/Desktop/Tally/ESA_Attributes/03.txt"

# systeminfo <- getSysInfo()
# currentdirectory <- as.character(systeminfo[systeminfo$Name == "Directory", "Value"])
# setwd(currentdirectory)

# filesourcepath <- paste(getwd(), "/file3.txt", sep = "") 
# filepath <-  paste(getwd(), "/03.txt", sep = "") 

noncechar <-  stri_rand_strings(n=1 , length = 24, pattern = "[A-Za-z0-9]")
nonce <- charToRaw(noncechar)
url <- "http://13.232.35.8:8080/known?id=1&count=3&nonce="
url <- paste(url, noncechar , sep = "" )
key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))

encrypt <- function(filesourcepath, filepath, key, nonce, noncechar, url){
  filecontent <- read_file(filesourcepath)
  #file.remove(filesourcepath)
  sfilecontent <- serialize(filecontent, NULL)
  filecontent <- data_encrypt(sfilecontent, key, nonce)
  filecontent <- as.vector(filecontent)
  writeBin(filecontent, filepath)
  xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
}


encrypt(filesourcepath, filepath, key,nonce, noncechar, url)


filepathofencryptedfile <- "C:/Users/user/Desktop/Tally/ESA_Attributes/04.txt"
filepathofdecryptedfile <- "C:/Users/user/Desktop/Tally/ESA_Attributes/file4.txt"

# filepathofencryptedfile <- paste(getwd(), "/04.txt", sep = "" ) 
# filepathofdecryptedfile <- paste(getwd(), "/file4.txt", sep = "") 

key <- hash(charToRaw(toString(as.character("this is a key"), width = NULL )))
url <- "http://13.232.35.8:8080/known?id=1&count=1&nonce=a"
xdata <- getURL(url = url,verbose=TRUE, .encoding = 'UTF-8')
noncechar <- stringi::stri_extract_all_regex(xdata, '(?<=").*?(?=")')[[1]]
nonce <- charToRaw(noncechar)

write.csv(cars,"C:/Users/user/Desktop/Tally/ESA_Attributes/cars.csv")
