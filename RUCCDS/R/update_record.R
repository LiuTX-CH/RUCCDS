#' @title Update the extant sign-in records
#' @description Update the record with a new sign-in form
#' @details The record and the sign-in form should be the fixed formats
#' @param record An object read by readxl package
#' @param form A fixed format data frame with 2 columns
#' @param order The order for the new sign-in form
#' @return A data frame used for storage
#' @export
#'

update_record <- function(record, form, order){
  ord <- order + 2
  record <- as.data.frame(record)
  form <- as.data.frame(form)

  record[,ord] <- NA
  names(record)[ord] <- paste0("第",order,"讲")

  for (j in 1:nrow(form)) {
    if (form[j,1] %in% record[,1]) {
      record[record$name == form[j,1],ord] <- order
    } else {
      newrow <- nrow(record)+1
      record[newrow,] <- NA
      record[newrow,1] <- form[j,1]
      record[newrow,2] <- form[j,2]
      record[newrow,ord] <- order
    }
  }
  return(record)
}


