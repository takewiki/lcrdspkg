#' 生成隔离条码模板
#'
#' @param conn 连接放在主数据库中
#'
#' @return 返回列表数据
#' @export
#'
#' @examples
#' barcode_ban_tpl()
barcode_ban_tpl <- function(conn=tsda::conn_rds('lcrds')) {
  sql <- paste0("SELECT  [FBarcode] ,[FChartNumber]  FROM [rds_barcode_banned_tpl]")
  res <- tsda::sql_select(conn,sql)
  names(res) <- c('二维码','图号')
  res2 <- list(res)
  names(res2) <- '隔离条码模板'
  return(res2)

}


#' 读取条码隔离模板信息
#'
#' @param file 文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' barcode_ban_readFile()
barcode_ban_readFile <- function(file="data-raw/条码隔离区下载模板.xlsx") {

  barCode_ban_data <- readxl::read_excel(file,
                                 sheet = "隔离条码模板")
  names(barCode_ban_data) <- c('FBarcode','FChartNumber')
  return(barCode_ban_data)

}

#' 条码隔离更新数据
#'
#' @param conn 连接
#' @param data 数据
#'
#' @return 无
#'
#' @examples
#' barcode_ban_updateDB()
barcode_ban_updateDB <- function(conn=tsda::conn_rds('LCERP'),data) {
  #上传数据
  tsda::upload_data(conn = conn,table_name = 'rds_barcode_banned_input',data = data)
  #更新数据,删除已经存在的数据
  sql_upd <- paste0("delete from rds_barcode_banned
where FBarcode in
(select FBarcode from  rds_barcode_banned_input)")
  tsda::sql_update(conn,sql_upd)
  #插入数据
  sql_ins <- paste0("insert into rds_barcode_banned
select *  from  rds_barcode_banned_input")
  tsda::sql_update(conn,sql_ins)
  #清空临时表
  sql_del <- paste0("truncate table rds_barcode_banned_input")
  tsda::sql_update(conn,sql_del)

}


#' 读取数据添加到隔离区
#'
#' @param file 文件
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' barCode_ban_add()
barCode_ban_add <- function(file="data-raw/条码隔离区下载模板.xlsx",conn=tsda::conn_rds('LCERP')) {
  data <- barcode_ban_readFile(file)
  ncount <- nrow(data)
  if (ncount >0){
    data$FDeleted <- rep(1,ncount)
    data$FDate <-rep(as.character(Sys.Date()),ncount)
    #更新数据
    barcode_ban_updateDB(conn,data)
  }

  return(data)

}


#' 读取数据移除到隔离区
#'
#' @param file 文件
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' barCode_ban_rm()
barCode_ban_rm <- function(file="data-raw/条码隔离区下载模板.xlsx",conn=tsda::conn_rds('LCERP')) {
  data <- barcode_ban_readFile(file)
  ncount <- nrow(data)
  if (ncount >0){
    data$FDeleted <- rep(0,ncount)
    data$FDate <-rep(as.character(Sys.Date()),ncount)
    #更新数据
    barcode_ban_updateDB(conn,data)
  }
  return(data)

}




#' 查询已经被禁用的条码
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' barcode_banned_query()
barcode_banned_query <- function(conn=tsda::conn_rds('LCERP')) {
  sql <- paste0(" select FBarcode,FChartNumber  from  rds_barcode_banned_deleted")
  res <- tsda::sql_select(conn,sql)
  return(res)

}





#' 查询已经被禁用的条码
#'
#' @param conn 连接
#' @param FStartDate 开始日期
#' @param FEndDate  结束日志
#' @param FChartNumber 图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' barcode_banned_query()
barcode_banned_query2 <- function(conn=tsda::conn_rds('LCERP2'),
                                 FStartDate='2021-01-01',
                                 FEndDate='9999-12-31',
                                 FChartNumber ='YX201B497-02'
                                 ) {

  if (FChartNumber == ''){
    sql <- paste0("select * from  rds_barcode_banned
where FDate >= '",FStartDate,"' and FDate <='",FEndDate,"'
and FDeleted =1 ")
  }else{
    sql <- paste0("select * from  rds_barcode_banned
where FDate >= '",FStartDate,"' and FDate <='",FEndDate,"'
and FDeleted =1 and FChartNumber ='",FChartNumber,"'")
  }

  res <- tsda::sql_select(conn,sql)
  ncount = nrow(res)
  if(ncount >0){
    names(res) <- c('内部二维码','图号','禁用标记','禁用日期')
  }
  return(res)

}
