#' @title Search participation records
#' @description If someone deliver a application for certificate, this function can be used to confirm their lectures participations.
#' @param record An object read by readxl package
#' @param name A fixed format data frame with 2 columns
#' @return Information of a specific applicant
#' @export
#'

search_record <- function(record, name){
  record <- as.data.frame(record)
  if (name %in% record[,1]) {
    item <- c(t(record[record$name == name,]))
    item <- item[is.na(item) == F]
    participation <- item[-c(1,2)]
    name <- item[1]
    phone <- item[2]
    times <- length(participation)
    cat(paste0("姓名：",name,"\n"))
    cat(paste0("手机号：",phone,"\n"))
    cat(paste0("此同学一共参与",times,"次讲座\n"))
    cat("此同学参与的讲座场次如下：\n")
    for (i in 1:length(participation)) {
      cat(paste0("    第",participation[i],"讲\n"))
    }
  } else {
    cat("暂无此同学的讲座参与记录","\n")
    cat("请确认姓名拼写是否正确")
  }
}
