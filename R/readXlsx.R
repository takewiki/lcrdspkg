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


#' 获取BOM页答名称
#'
#' @param file 文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' lc_bom_sheetName()
lc_bom_sheetName <- function(file){
  sheetNames_all <- tsda::excel_getSheetNames(file)
  exclude_sheetName <-lc_exclude_sheetNames()
  res <- sheetNames_all[!sheetNames_all %in% exclude_sheetName]
  return(res)

}




