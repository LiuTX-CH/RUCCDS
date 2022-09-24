#' @title Update penalty blacklist before the lecture
#' @description To update the penalty blacklist when someone had been punished（removed from the enrollment form）
#' @param blacklist The CDS blacklist: An object read by readxl package
#' @param form Enrollment form of CDS lectures: An object read by readxl package
#' @return Updated blacklist
#' @export
#'
preupdate_blacklist <- function(blacklist, form){
  blacklist <- as.data.frame(blacklist)
  pen <- blacklist[blacklist$penalty > 0,]
  form <-  as.data.frame(form)
  for(j in 1:nrow(form)){
    if(form[j, 1]%in%pen$name == T){
      blacklist[grep(form[j, 1], blacklist$name), "penalty"] <-
        as.numeric(blacklist[grep(form[j, 1], blacklist$name), "penalty"]) -1
    }
  }
  return(blacklist)
}
