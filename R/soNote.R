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
#'
#' @return 返回值
#' @export
#'
#' @examples
#' soNote_uploadDB()
soNote_uploadDB <- function(file='data-raw/订单备注模板.xlsx',conn=tsda::conn_rds('lcrds')) {
  data <- soNote_read(file = file)

  ncount <- nrow(data)
  if(ncount >0){
    names(data) <- c('FContact','FSoNo','FChartNo','FNote','FFlag','FComfirmed')
    data$FFlag[data$FFlag == TRUE] <- 1
    data$FFlag[data$FFlag == FALSE] <-0
    #add the try
    try(tsda::upload_data(conn = conn,table_name = 't_lcrds_soNote',data = data))
  }


}

