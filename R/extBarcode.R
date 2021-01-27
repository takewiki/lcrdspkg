#' 读取外部条码信息
#'
#' @param file 文件
#'
#' @return 返回值
#' @include soSplit.R
#' @export
#'
#' @examples
#' extBarcode_read()
extBarcode_read <- function(file="data-raw/外部订单模板.xlsx",FCalcNo='LSD001',lang='cn') {
  res <- readxl::read_excel(file);
  res <- res[ ,c('订单号',	'物料号'	,'二维码','备注')];
  res$FCalcNo <- FCalcNo;
  names(res) <-c('FSoNo','FChartNo','FBarcode','FNote','FCalcNo')
  res$FNote <- tsdo::na_replace(res$FNote,'')
  res$FNote <- as.character(res$FNote)
  res$FSoNo <- as.character(res$FSoNo)
  #增值对订单号~有处理
  res$FChartNo <- soSplit(res$FChartNo)
  if (lang == 'cn'){
    names(res) <- c('订单号','物料号(图号)','二维码','备注','运算单号');
  }

  return(res)
}


#' 获取最大运算号
#'
#' @param conn 连接
#'
#' @return 返顺值
#' @export
#'
#' @examples
#' extBarcode_newId()
extBarcode_newId <- function(conn=tsda::conn_rds('lcrds')) {
  sql <- paste0("select isnull(max(FCalcNo),0)+1  as FCalcNo from  takewiki_ext_barcode")
  r <- tsda::sql_select(conn,sql)
  res <- r$FCalcNo
  return(res)


}
