#' @title Update penalty blacklist before the lecture
#' @description To update the penalty blacklist when someone had been punished（removed from the meal.notice）
#' @param blacklist The CDS blacklist: An object read by readxl package
#' @param meal.notice meal.notice of CDS lectures: An object read by readxl package
#' @return Updated blacklist
#' @export
#'
preupdate_blacklist <- function(blacklist, meal.notice){
  blacklist <- as.data.frame(blacklist)
  pen <- blacklist[blacklist$penalty > 0,]
  meal.notice <-  as.data.frame(meal.notice)
  for(j in 1:nrow(meal.notice)){
    if(meal.notice[j, 1]%in%pen$name == T){
      blacklist[grep(meal.notice[j, 1], blacklist$name), "penalty"] <-
        as.numeric(blacklist[grep(meal.notice[j, 1], blacklist$name), "penalty"]) -1
    }
  }
  return(blacklist)
}
