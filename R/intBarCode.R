#' 查询内部条码
#'
#' @param fchartNo 图号
#' @param conn 添加连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' query_barcode_chartNo()
query_barcode_chartNo <- function(conn=tsda::conn_rds('lcrds'),fchartNo ='P207012C134G01'){


  sql <- paste0("select FBarcode as '二维码',FChartNumber '图号',FBillNo as '生产任务单号' from  takewiki_mo_barcode
where FChartNumber ='",fchartNo,"'")
  print(sql)
  r <- tsda::sql_select(conn,sql)
  return(r)

}
