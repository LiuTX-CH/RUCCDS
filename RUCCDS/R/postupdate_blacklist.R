#' @title Update blacklist after the lecture
#' @description To put someone broke the rules on the CDS blacklist
#' @param blacklist The CDS blacklist: An object read by readxl package
#' @param newblack Names needed to be put on blacklist: An object read by readxl package
#' @return Updated blacklist
#' @export

postupdate_blacklist <- function(blacklist, newblack){
  blacklist <- as.data.frame(blacklist)
  newblack <-  as.data.frame(newblack)
  for(j in 1:nrow(newblack))
    if(newblack[j, 1]%in%blacklist$name == F){
      blacklist[nrow(blacklist)+1,] <- c(newblack[j, 1],1,1)
    } else {
      blacklist[grep(newblack[j, 1], blacklist$name), "penalty"] <-
        as.numeric(blacklist[grep(newblack[j, 1], blacklist$name), "penalty"]) + 1
      blacklist[grep(newblack[j, 1], blacklist$name), "sum"] <-
        as.numeric(blacklist[grep(newblack[j, 1], blacklist$name), "sum"]) + 1
    }
  return(blacklist)
}
