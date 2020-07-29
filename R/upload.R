#针对上传的图号进行处理-------

#' 针对上传的图号完成处理后设置标志为1
#'
#' @param conn 连接
#' @param FchartNo  主图号
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' db_bom_setUpdate()
db_bom_setUpdate <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672'){
  sql <- paste0("update  a  set a.FIsDo = 1  from t_lcrds_uploadLOG  a  where FIsDo =0 and FchartNo ='",FchartNo,"'")
  tsda::sql_update(conn,sql)

}
