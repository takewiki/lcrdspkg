#' 读取图号及物料对照表
#'
#' @param conn 连接
#' @param file 文件
#' @param sheet 页签
#'
#' @return 返顺值
#' @export
#'
#' @examples
#' read_chartMtrlMapping()
read_chartMtrlMapping <- function(conn=tsda::conn_rds('lcrds'),file="data-raw/图号物料匹配表.xlsx",sheet = "图号物料匹配表") {
  #library(readxl)
  chartMtrlMapping <- readxl::read_excel(file,
                                 sheet = sheet)
  chartMtrlMapping$DM单号 <- tsdo::na_replace(chartMtrlMapping$DM单号,'')
  chartMtrlMapping$图号 <- tsdo::na_replace(chartMtrlMapping$图号,'')
  chartMtrlMapping$G番 <- tsdo::na_replace(chartMtrlMapping$G番,'')
  chartMtrlMapping$L番 <- tsdo::na_replace(chartMtrlMapping$L番,'')
 #print(chartMtrlMapping)
  #上传数据
  tsda::db_writeTable(conn,table_name = 't_lcrds_chartMtrlMap_input',r_object = chartMtrlMapping,append = T)

  #更新数据，删除已有数据

  sql_bak <- paste0("
delete  from t_lcrds_chartMtrlMap
where FItemIndex
in
(select FItemIndex  from t_lcrds_chartMtrlMap_input)")
  tsda::sql_update(conn,sql_bak)
  #同步数据
  sql_sync <- paste0("
insert into t_lcrds_chartMtrlMap
select * from t_lcrds_chartMtrlMap_input")
  tsda::sql_update(conn,sql_sync)
  #删除数据
  sql_del <-paste0("
delete from t_lcrds_chartMtrlMap_input")
  tsda::sql_update(conn,sql_del)
  return(chartMtrlMapping)

}


