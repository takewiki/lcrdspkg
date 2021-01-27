#' 订单备注测试数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#'soNote_data()
soNote_data <- function() {
  FContact <-c('A','A','B')
  FSoNo <- c('123','123','456')
  FChartNo <- c('P203031A112123','P203031A112543','abc')
  FNote <-c('123','123','')
  res <-data.frame(FContact,FSoNo,FChartNo,FNote,stringsAsFactors = FALSE)
  return(res)

}

#' 订单备注模板
#'
#' @return 返回列表
#' @export
#'
#' @examples
#' soNote_data_tpl()
soNote_data_tpl <- function() {
  data <- soNote_data()
  names(data) <- c('合同号','订单号','图号','备注')
  res <- list(data)
  names(res) <-'订单备注'
  return(res)


}

#' 自动判断相应的数据
#'
#' @param data 数据
#' @param field_main 值
#' @param key 主键
#'
#' @return 返回值
#' @export
#'
#' @examples
#' soNote_flag()
soNote_flag <- function(data=soNote_data(),field_main='FChartNo',key='P203031A112'){
  words <- data[,field_main,drop=TRUE]
  logi_flag <- stringr::str_detect(words,key)
  #print(logi_flag)
  data$FLag <-logi_flag
  #print(data$FLag)
  data$FComfirmed  <- ''
  data$FComfirmed[data$FLag == TRUE] <- key
  return(data)
}



#' 读取模板数据
#'
#' @param file 文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' soNote_read()
soNote_read <- function(file='data-raw/订单备注模板.xlsx'){
  library(readxl)
  data_raw <- read_excel(file,sheet = "订单备注")
  data_raw <- as.data.frame(data_raw)
  field_sel <-c('合同号','订单号','图号','备注')
  data <- data_raw[,field_sel]
  res <- soNote_flag(data = data,field_main = '图号',key='P203031A112')
  names(res) <-c('合同号','订单号','图号','备注','标记','复核')
  return(res)

}

#' 订单备注上传数据库
#'
#' @param file 文件名
#' @param conn 连接
#' @param table_name 表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' soNote_uploadDB()
soNote_uploadDB <- function(file='data-raw/订单备注模板.xlsx',table_name='t_lcrds_soNote',conn=tsda::conn_rds('lcrds')) {
  data <- soNote_read(file = file)

  ncount <- nrow(data)
  if(ncount >0){
    names(data) <- c('FContact','FSoNo','FChartNo','FNote','FFlag','FComfirmed')
    data$FFlag[data$FFlag == TRUE] <- 1
    data$FFlag[data$FFlag == FALSE] <-0
    #add the try
    try(tsda::upload_data(conn = conn,table_name = table_name,data = data))
  }


}


#' 获取运算单最大号
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' soNote_getMaxCalcNo()
soNote_getMaxCalcNo <- function(conn=tsda::conn_rds('lcrds')) {
  sql <- paste0(" select isnull(max(FCalcNo),0) as  FCalcNo  from  v_lcrds_soNoteQuery")
  r <- tsda::sql_select(conn,sql)
  res <- r$FCalcNo
  return(res)

}


#' 订单备注信息视图查询----
#'
#' @param conn  连接
#' @param FSoNo1 开始外部订单号
#' @param FSoNo2 结束外部订单号
#' @param FCalcNo 运算号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' soNote_view_query()
soNote_view_query <- function(conn=tsda::conn_rds('lcrds'),FSoNo1 ='117583137' ,FSoNo2 ='117583139',FCalcNo = soNote_getMaxCalcNo()) {
  sql <- paste0("  SELECT  [FSoNo] 外部订单号
      ,[FChartNo] 订单图号
      ,[FBarcode] 外部条码
      ,[FNote]   订单备注
      ,[FCalcNo]  运算单号
      ,[FNote_Tech] 技术备注
      ,[FChartNo_112] 图号112
      ,[FContact]  合同号
      ,[FchartNo_Tech] 图号_技术部
      ,[FFlag_112] 是否112
      ,[FNote_Mfg] 备注_生产
  FROM [lcrds].[dbo].[v_lcrds_soNoteQuery]
  where FSoNo >='",FSoNo1,"' and FSoNo<='",FSoNo2,"'
  and FCalcNo =  ",FCalcNo)
  res <- tsda::sql_select(conn,sql)
  return(res)

}

