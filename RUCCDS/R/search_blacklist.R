#' @title Search blacklist and update enrollment form
#' @description To remove names on the blacklist from the enrollment form. There is no one need to be removed if no object returned.
#' @param blacklist The CDS blacklist: An object read by readxl package
#' @param form Enrollment form of CDS lectures: An object read by readxl package
#' @return Updated enrollment form
#' @export
#'
search_blacklist <- function(blacklist, form){
  blacklist <- as.data.frame(blacklist)
  pen <- blacklist[blacklist$penalty > 0,]
  form <-  as.data.frame(form)
  name.list <- 1:2
  for(j in 1:nrow(form)){
    if(form[j, 1]%in%pen$name == T){
      newform <- form[-j, ]
      name.list <- append(name.list, form[j, 1])
    }
  }
  name.list <- name.list[-c(1,2)]
  cat(paste0(c("下列人员需要通知取消用餐资格一次：",name.list),"\n",collapse = " "))
  cat(paste0("新名单需要递补",length(name.list),"人"))
  return(newform)
}
