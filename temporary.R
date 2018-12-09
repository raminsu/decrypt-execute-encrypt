fetchingvoucher_updates <- function(voucherrequestpath1, voucherrequestpath2
                                    , pathofvoucherfile1 , pathofvoucherfile2, pathofvoucherfile3){
  while (breakup) {
    if(file.exists(pathofvoucherfile1) && file.exists(pathofvoucherfile2) 
       && file.exists(pathofvoucherfile3)){
      xmldf_voucher_check1 <<- read.csv(pathofvoucherfile1)
      xmldf_voucher_check2 <<- read.csv(pathofvoucherfile2)
      xmldf_voucher_check3 <<- read.csv(pathofvoucherfile3)
      xmldf_voucher_check <<- rbind.data.frame(xmldf_voucher_check1[,c("MASTERID", "ALTERID")]
                                               ,xmldf_voucher_check2[,c("MASTERID", "ALTERID")]
                                               ,xmldf_voucher_check3[,c("MASTERID", "ALTERID")])
      
      xmldf_voucher_detail1 <<- read.csv(pathofvoucherfile1)
      xmldf_voucher_detail2 <<- read.csv(pathofvoucherfile2)
      xmldf_voucher_detail3 <<- read.csv(pathofvoucherfile3)
      
    }else{
      xmldf_voucher_check <<- xmldf_voucher[FALSE,]
      xmldf_voucher_detail1 <<- NULL
      xmldf_voucher_detail2 <<- NULL
      xmldf_voucher_detail3 <<- NULL
    }
    xmldf_voucher <- convertdfcolumnstonumeric(xmldf_voucher, c("MASTERID", "ALTERID"))
    xmldf_voucher_check <- convertdfcolumnstonumeric(xmldf_voucher_check, c("MASTERID", "ALTERID"))
    xmldf_voucher_retreive <- dplyr::setdiff(xmldf_voucher[,c("MASTERID", "ALTERID")], xmldf_voucher_check[,c("MASTERID", "ALTERID")])
    
    
    xmlrequestfile1 <- read_file(voucherrequestpath1)
    masterid <- xmldf_voucher_retreive$MASTERID
    xmlrequestfile2 <- read_file(voucherrequestpath2)
    
    allvouchercollectioncsv <- as.data.frame(NULL)
    xmlrequestpath1 <- read_file(voucherrequestpath1)
    xmlrequestpath2 <- read_file(voucherrequestpath2)
    
    fetching_single_voucher <- function(xmlrequestpath1, masteridnumber , xmlrequestpath2){
      xmlsingle_voucher <<- NULL
      allvoucherheader1 <<- NULL
      allvoucherheader2 <<- NULL
      allvoucherheader3 <<- NULL
      if(readcompanynameandcompare(pathofcompanynamefile)==TRUE){
        xmlrequest <- paste(xmlrequestpath1, masterid[i] , xmlrequestpath2 , sep = "")
        allvouchercollection <- getxmlresponse(xmlrequest)
        allvouchercollection <- xmlstringsubset(allvouchercollection, "<DATA>" , "</DATA>")
        allvouchercollection <- clrspecialcharacters_ampersand(allvouchercollection)
        allvoucherheader <- convertnestedxmltodf(allvouchercollection, "//DATA")
        allvoucherheader <- allvoucherheader[,1:which(colnames(allvoucherheader)=="VOUCHERKEY")]
        allvoucherledgelineitems <- convertnestedxmltodf(allvouchercollection, "//LEDGERENTRIES.LIST")
        allvoucherinventorylineitems <- convertnestedxmltodf(allvouchercollection, "//INVENTORYENTRIES.LIST")
        
        if(class(allvoucherledgelineitems) == "NULL"){
          allvoucherledgelineitems <- convertnestedxmltodf(allvouchercollection, "//ALLLEDGERENTRIES.LIST")
        }
        if(class(allvoucherinventorylineitems) == "NULL"){
          allvoucherinventorylineitems <- convertnestedxmltodf(allvouchercollection, "//ALLINVENTORYENTRIES.LIST")
          allvoucherinventorylineitemsbatchallocations <- convertnestedxmltodf(allvouchercollection, "//BATCHALLOCATIONS.LIST")
        }
        
        if(!is.null(allvoucherledgelineitems)){
          colnames(allvoucherledgelineitems) <- paste(colnames(allvoucherledgelineitems), "ledgeritems" , sep = "")
          allvoucherheader1 <<- cbind.data.frame(allvoucherheader, allvoucherledgelineitems)
        }
        
        if(!is.null(allvoucherinventorylineitems)){
          colnames(allvoucherinventorylineitems) <- paste(colnames(allvoucherinventorylineitems),"inventoryitems", sep = "")
          allvoucherheader2 <<- cbind.data.frame(allvoucherheader, allvoucherinventorylineitems)
        }
        
        if(!is.null(allvoucherinventorylineitemsbatchallocations)){
          allvoucherheader3 <<- cbind.data.frame(allvoucherheader, allvoucherinventorylineitemsbatchallocations)
        }
        
        #xmlsingle_voucher <<- rbind.fill(xmlsingle_voucher, allvoucherheader)
      }}
    
    
    if(length(masterid)!=0){
      if(length(masterid) < 100){
        breakup <- FALSE}
      if(length(masterid) > 100){
        masterid <- masterid[1:100]}
      for(i in 1:length(masterid)){
        fetching_single_voucher(xmlrequestfile1, masterid[i] , xmlrequestfile2)
        if(is.null(NROW(xmldf_voucher_detail1))) {
          #xmldf_voucher_detail <<- rbind.fill(xmldf_voucher_detail, xmlsingle_voucher)
          xmldf_voucher_detail1 <<- rbind.fill(xmldf_voucher_detail1, allvoucherheader1)
        } else if(length(which(xmldf_voucher_detail1$MASTERID == masterid[i])) != 0){
          # xmldf_voucher_detail <<- xmldf_voucher_detail[-1 * (which(xmldf_voucher_detail$MASTERID == masterid[i])),] 
          # xmldf_voucher_detail <<- rbind.fill(xmldf_voucher_detail, xmlsingle_voucher)
          
          xmldf_voucher_detail1 <<- xmldf_voucher_detail1[-1 * (which(xmldf_voucher_detail1$MASTERID == masterid[i])),] 
          xmldf_voucher_detail1 <<- rbind.fill(xmldf_voucher_detail1, allvoucherheader1)
        } else {
          #xmldf_voucher_detail <<- rbind.fill(xmldf_voucher_detail, xmlsingle_voucher)
          xmldf_voucher_detail1 <<- rbind.fill(xmldf_voucher_detail1, allvoucherheader1)
        }
        
        
        if(is.null(NROW(xmldf_voucher_detail2))) {
          #xmldf_voucher_detail <<- rbind.fill(xmldf_voucher_detail, xmlsingle_voucher)
          xmldf_voucher_detail2 <<- rbind.fill(xmldf_voucher_detail2, allvoucherheader2)
        } else if(length(which(xmldf_voucher_detail2$MASTERID == masterid[i])) != 0){
          # xmldf_voucher_detail <<- xmldf_voucher_detail[-1 * (which(xmldf_voucher_detail$MASTERID == masterid[i])),] 
          # xmldf_voucher_detail <<- rbind.fill(xmldf_voucher_detail, xmlsingle_voucher)
          xmldf_voucher_detail2 <<- xmldf_voucher_detail2[-1 * (which(xmldf_voucher_detail2$MASTERID == masterid[i])),] 
          xmldf_voucher_detail2 <<- rbind.fill(xmldf_voucher_detail2, allvoucherheader2)
        } else {
          #xmldf_voucher_detail <<- rbind.fill(xmldf_voucher_detail, xmlsingle_voucher)
          xmldf_voucher_detail2 <<- rbind.fill(xmldf_voucher_detail2, allvoucherheader2)
        }
        
        
        if(is.null(NROW(xmldf_voucher_detail3))) {
          #xmldf_voucher_detail <<- rbind.fill(xmldf_voucher_detail, xmlsingle_voucher)
          xmldf_voucher_detail3 <<- rbind.fill(xmldf_voucher_detail3, allvoucherheader3)
        } else if(length(which(xmldf_voucher_detail3$MASTERID == masterid[i])) != 0){
          # xmldf_voucher_detail <<- xmldf_voucher_detail[-1 * (which(xmldf_voucher_detail$MASTERID == masterid[i])),] 
          # xmldf_voucher_detail <<- rbind.fill(xmldf_voucher_detail, xmlsingle_voucher)
          xmldf_voucher_detail3 <<- xmldf_voucher_detail3[-1 * (which(xmldf_voucher_detail3$MASTERID == masterid[i])),] 
          xmldf_voucher_detail3 <<- rbind.fill(xmldf_voucher_detail3, allvoucherheader3)
        } else {
          #xmldf_voucher_detail <<- rbind.fill(xmldf_voucher_detail, xmlsingle_voucher)
          xmldf_voucher_detail3 <<- rbind.fill(xmldf_voucher_detail3, allvoucherheader3)
        }
        
        xmldf_voucher_detail1 <<- convertdfcolumnstonumeric(xmldf_voucher_detail1, c("MASTERID", "ALTERID"))
        xmldf_voucher_detail2 <<- convertdfcolumnstonumeric(xmldf_voucher_detail2, c("MASTERID", "ALTERID"))
        xmldf_voucher_detail3 <<- convertdfcolumnstonumeric(xmldf_voucher_detail3, c("MASTERID", "ALTERID"))
      }
    } 
    if(NROW(xmldf_voucher_detail1)!=0){
      write.csv(xmldf_voucher_detail1,pathofvoucherfile1, row.names = FALSE)}
    if(NROW(xmldf_voucher_detail2)!=0){
      write.csv(xmldf_voucher_detail2,pathofvoucherfile2, row.names = FALSE)}
    if(NROW(xmldf_voucher_detail3)!=0){
      write.csv(xmldf_voucher_detail3,pathofvoucherfile3, row.names = FALSE)}
    
  }
  
  # if(NROW(xmldf_voucher_detail1)!=0){
  #   xmldf_voucher_detail1$VOUCHERTYPENAME <- as.character(xmldf_voucher_detail1$VOUCHERTYPENAME)
  #   xmldf_voucher_detail1 <- dplyr::left_join(xmldf_voucher_detail1, vouchertype, by = "VOUCHERTYPENAME")
  # }
  # if(NROW(xmldf_voucher_detail2)!=0){
  #   xmldf_voucher_detail2$VOUCHERTYPENAME <- as.character(xmldf_voucher_detail2$VOUCHERTYPENAME)
  #   xmldf_voucher_detail2 <- dplyr::left_join(xmldf_voucher_detail2, vouchertype, by = "VOUCHERTYPENAME")
  # }
  # if(NROW(xmldf_voucher_detail3)!=0){
  #   xmldf_voucher_detail3$VOUCHERTYPENAME <- as.character(xmldf_voucher_detail3$VOUCHERTYPENAME)
  #   xmldf_voucher_detail3 <- dplyr::left_join(xmldf_voucher_detail3, vouchertype, by = "VOUCHERTYPENAME")
  # }
  
  listobtained <- as.list(NA)
  listobtained[[1]] <- xmldf_voucher_detail1
  listobtained[[2]] <- xmldf_voucher_detail2
  listobtained[[3]] <- xmldf_voucher_detail3
  listobtained[[4]] <- xmldf_voucher_retreive
  return(listobtained)
}