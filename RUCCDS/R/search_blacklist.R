#' @title Search blacklist and update meal.notice
#' @description To remove names on the blacklist from the meal.notice. There is no one need to be removed if no object returned.
#' @param blacklist The CDS blacklist: An object read by readxl package
#' @param meal.notice Enrollment meal.notice of CDS lectures: An object read by readxl package
#' @return Updated meal.notice
#' @export
#'
search_blacklist <- function(blacklist, meal.notice){
  blacklist <- as.data.frame(blacklist)
  pen <- blacklist[blacklist$penalty > 0,]
  meal.notice <-  as.data.frame(meal.notice)
  name.list <- 1:2
  for(j in 1:nrow(meal.notice)){
    if(meal.notice[j, 1]%in%pen$name == T){
      newmeal.notice <- meal.notice[-j, ]
      name.list <- append(name.list, meal.notice[j, 1])
    }
  }
  name.list <- name.list[-c(1,2)]
  cat(paste0(c("下列人员需要通知取消用餐资格一次：",name.list),"\n",collapse = " "))
  cat(paste0("新名单需要递补",length(name.list),"人"))
  return(newmeal.notice)
}
