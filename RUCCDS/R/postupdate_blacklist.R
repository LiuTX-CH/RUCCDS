#' @title Update blacklist after the lecture
#' @description To put someone broke the rules on the CDS blacklist
#' @param blacklist The CDS blacklist: An object read by readxl package
#' @param newblacklist Names needed to be put on blacklist: An object read by readxl package
#' @return Updated blacklist
#' @export

postupdate_blacklist <- function(blacklist, newblacklist){
  blacklist <- as.data.frame(blacklist)
  newblacklist <-  as.data.frame(newblacklist)
  for(j in 1:nrow(newblacklist))
    if(newblacklist[j, 1]%in%blacklist$name == F){
      blacklist[nrow(blacklist)+1,] <- c(newblacklist[j, 1],1,1)
    } else {
      blacklist[grep(newblacklist[j, 1], blacklist$name), "penalty"] <-
        as.numeric(blacklist[grep(newblacklist[j, 1], blacklist$name), "penalty"]) + 1
      blacklist[grep(newblacklist[j, 1], blacklist$name), "sum"] <-
        as.numeric(blacklist[grep(newblacklist[j, 1], blacklist$name), "sum"]) + 1
    }
  return(blacklist)
}
