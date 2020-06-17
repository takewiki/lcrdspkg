# library(tsda)
# file = "./data-raw/bom_src.xlsx"
#
#
# sheetNames <- tsda::excel_getSheetNames(file)



#' 不属G与L表的页签
#'
#' @return 返回值
#' @export
#'
#' @examples
#' lc_exclude_sheetNames()
lc_exclude_sheetNames <- function() {

  res <- c('DM清单','物料价格')
  return(res)

}




