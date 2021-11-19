#针对物料进行分配--------
#' 针对数据填写分配逻辑
#'
#' @param conn_rds 连接1
#' @param conn_erp 连接2
#' @param FCalcNo 计算号
#' @param FChartNo 图号
#' @param FNote_All 备注
#' @param n  行数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' extBacode_Allocate_ByEachNote()
extBarcode_Allocate_ByEachNote <- function(conn_rds=tsda::conn_rds('lcrds'),
                                      conn_erp=tsda::conn_rds('LCERP2'),
                                      FCalcNo=11,
                                      FChartNo='P207012C134G01',
                                      FNote_All='G116*G103,G116,G107*G110,G363*G168,',
                                      n=4) {

data_ext <-exrBarcode_getUnAllocated(conn = conn_rds,FCalcNo = FCalcNo,FChartNo = FChartNo,FNote_All = FNote_All,n = n)
print('step 1')
print(data_ext)
ncount_ext <- nrow(data_ext)
data_inner <-barcodeInner_getUnAllocated(conn = conn_erp,FChartNo = FChartNo,FNote_ERP = FNote_All,n = n)
print('step 2')
print(FNote_All == '503-标配整合,G103,G108/G110/G407,G363*G168,')
print(n == 1)
print(n == 4)
print(FChartNo)
print(data_inner)
ncount_inner <- nrow(data_inner)
if ((ncount_ext >0) &( ncount_inner >0)){
  res <- tsdo::allocate(data_ext,data_inner)
  print('step3')
  print(res)
  ncount <- nrow(res)
  if (ncount >0){
    res <- res[,c("FSoNo", "FChartNo","FNote" , "FBarcode_ext","FBarcode_inner",   "FCalcNo" )]

    try(tsda::upload_data(conn = conn_erp,table_name = 'takewiki_barcode_allocate_auto',data = res))
  }

}else{
  res <- NULL
}


return(res)


}


#' 条码进行自动匹配
#'
#' @param conn_rds 链接1
#' @param conn_erp 链接2
#'
#' @return 返回值
#' @export
#'
#' @examples
#' extBarcode_AllocateALL()
extBarcode_AllocateALL <-function(conn_rds=tsda::conn_rds('lcrds'),
                                  conn_erp=tsda::conn_rds('LCERP2')
                                 ){
  data <-extBarcode_getNoteCount_currentCalc(conn=conn_rds)
  print(data)
  ncount <-  nrow(data)
  print( ncount)
  calcNo <-extBarcode_MaxCalcNo(conn=conn_rds)
  if(ncount >0){
    lapply(1:ncount, function(i){
      print(paste0('charpter',i))
      # fix the bug
      try({
        extBarcode_Allocate_ByEachNote(conn_rds = conn_rds,conn_erp = conn_erp,FCalcNo = calcNo,
                                       FChartNo = data$FChartNo[i],FNote_All = data$FNote_All[i],n = data$FNoteCount[i])
      })



    })
  }



}


#' 获取条码分配结果
#'
#' @param conn_rds 连接1
#' @param conn_erp 连接2
#'
#' @return 返回值
#' @export
#'
#' @examples
#' extBarcode_AllocateResult()
extBarcode_AllocateResult <-function(conn_rds=tsda::conn_rds('lcrds'),
                                     conn_erp=tsda::conn_rds('LCERP2')
)
{
  #获取最大号
  calcNo <-extBarcode_MaxCalcNo(conn=conn_rds)
  # 添加排序规则
  sql <- paste0(" select FSoNo,FChartNo,FBarcode_ext,FBarcode_inner,FNote,FPrdName from vw_takewiki_barcode_allocate_auto
 where FCalcNo = ",  calcNo  ,"    order by  FBarcode_inner   ")
  res <- tsda::sql_select(conn = conn_erp,sql)

  names(res) <-c('销售订单号','图号','外部二维码','内部二维码','订单信息备注','品名')
  return(res)





}
