# 1.0销售订单备注，带外部条码号******----------
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





#' 获取当前最大运行号
#'
#' @param conn 连接信息
#'
#' @return 返回值
#' @export
#'
#' @examples
#' extBarcode_MaxCalcNo()
extBarcode_MaxCalcNo <- function(conn=tsda::conn_rds('lcrds')) {
  sql <- paste0("select isnull(max(FCalcNo),0)  as FCalcNo from  takewiki_ext_barcode")
  r <- tsda::sql_select(conn,sql)
  res <- r$FCalcNo
  return(res)


}




#' 按指定运算号查询外部条件
#'
#' @param conn 连接
#' @param calcNo 运算号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' extBarcode_QueryByCalcNo()
extBarcode_QueryByCalcNo <- function(conn=tsda::conn_rds('lcrds'),calcNo=11) {
  sql <- paste0("select FSoNo,FChartNo,FBarcode,FNote from takewiki_ext_barcode
where FCalcNo = ",calcNo)
  res <- tsda::sql_select(conn,sql)
  return(res)

}




#' 获取当前运算单号的外部条件
#'
#' @param conn 连接
#'
#' @return 返回结果
#' @export
#'
#' @examples
#' extBarcode_Query_CurrentCalcNo()
extBarcode_Query_CurrentCalcNo <- function(conn=tsda::conn_rds('lcrds')) {
  #获取当前最大号
  calcNo <-extBarcode_MaxCalcNo(conn=conn)
  #查询数据
  sql <- paste0("select FSoNo,FChartNo,FBarcode,FNote from takewiki_ext_barcode
where FCalcNo = ",calcNo)
  res <- tsda::sql_select(conn,sql)
  return(res)

}


#技术备注：这是非常重要的一部分内容********----------
# 感觉缺少条码信息
#' 外部订单查询技术备注
#'
#' @param conn 连接
#'
#' @return 返回查询结果
#' @export
#'
#' @examples
#' extcode_query_TechNote()
extBarcode_query_TechNote <- function(conn=tsda::conn_rds('lcrds')) {
  sql <- paste0("select FContact, FSoNo,FChartNo,FNote,FComfirmed from  t_lcrds_soNote
where FFlag =1")
  res <- tsda::sql_select(conn,sql)
  return(res)

}




#3.0 生产订单备注原则上不参与匹配*******----------
# 感觉缺少条码信息
#' 生产订单备注
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' extcode_query_mfgNote()
extBarcode_query_mfgNote <- function(conn=tsda::conn_rds('lcrds')) {
  sql <- paste0("select FContact, FSoNo,FChartNo,FNote,FComfirmed from  t_lcrds_soNote2
where FFlag =1")
  res <- tsda::sql_select(conn,sql)
  return(res)

}


#' 获取当前运算号的所有备注信息
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' extBarcode_query_NoteALL()
extBarcode_query_NoteALL <- function(conn=tsda::conn_rds('lcrds')) {
  #获取当前最大号
  calcNo <-extBarcode_MaxCalcNo(conn=conn)
  sql <- paste0("SELECT  [FSoNo]
      ,[FChartNo]
      ,[FBarcode]
      ,[FNote]  as FNote_So
      ,[FCalcNo]
      ,[FNote_Tech]
      ,[FChartNo_112]
      ,[FContact]

      ,[FFlag_112]
	  ,isnull(FNote,'') + isnull(FNote_Tech,'') as  FNote_All

  FROM [lcrds].[dbo].[v_lcrds_soNoteQuery]
  where FCalcNo =  ",calcNo)

  res <- tsda::sql_select(conn,sql)
  return(res)

}



