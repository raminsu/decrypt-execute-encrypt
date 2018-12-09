# myfile.R

Sys.setenv(TZ='Asia/Calcutta') 
setwd("C:/Users/user/Desktop/Tally/ESA_Attributes")
pathoftablelogfile <- paste( getwd() , "/ESA_Param/table.csv", sep = "")

#' @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

# id = 1
# count = 11
# table[1,11] <- NA


#' @get /known
normalMean <- function(id = as.integer(999), count = as.integer(3), nonce = as.character("a")){
  if(TRUE){

    table <- read.csv(pathoftablelogfile,  header = TRUE, stringsAsFactors = FALSE)
    id <- as.integer(id)
    
    count <- as.integer(count)
    #because there are first two columns namely "id" and "type"
    count = count + 2
    
    nonce <- as.character(nonce)
    newnonce <- nonce

    #intersect is used to find the row number which satisfies both id and type columns
    rownumbernonce <- intersect(which(table[,"id"] == id), which(table[, "type"] == "nonce"))
    oldnonce <- table[rownumbernonce, count]
    if(newnonce!="a"){
      #if nonce in get request == 1 then only send the nonce do not overwrite 
    newnonce <- nonce
    table[rownumbernonce, count] <- newnonce
    }


    #intersect is used to find the row number which satisfies both id and type columns
    rownumberstamp <- intersect(which(table[,"id"] == id), which(table[, "type"] == "stamp"))
    table[rownumberstamp, count] <- format(Sys.time(), "%X  %d %a %b %Y")
    
    #set lock to TRUE if the time difference between any column is greater than 10 
    if(count ==3){
      #first time request do nothing
    }else{
    for(i in 4:count){ 
        check = as.numeric(abs(strptime(table[rownumberstamp,i], "%X  %d %a %b %Y" ) - strptime(table[rownumberstamp,i-1], "%X  %d %a %b %Y" )), units="secs")
        if(!is.na(check)){
        if(check>10){
        table[rownumberstamp, "lock"] = TRUE
        }else{
          #nothing
         }
        }
      #nothing
    }
    }
    
  } 
  write.csv(table, pathoftablelogfile, row.names = FALSE)
  list(oldnonce)
}




