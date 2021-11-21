#'查看最近日志记录
#'
#' @param conn_erp ERP
#' @param topN 第N次
#'
#' @return 返回值
#' @export
#'
#' @examples 返回值
#' log_getCurrent()
log_getDetail <-function(
  conn_erp=tsda::conn_rds('LCERP2'),
  topN = 1){

  sql <- paste0("select * from vw_takewiki_barcode_allocate_auto
where FCalcNo in
(
select   distinct top   ",topN,"    FCalcNo  from takewiki_barcode_allocate_auto
order by FCalcNo desc  ) order by FCalcNo,FPrdName ")
  data =tsda::sql_select(conn_erp,sql)
  ncount =nrow(data)
  if(ncount >0){
    names(data) <-c('外部订单号','图号','订单备注','外部二维码','内部二维码','计算号','图号')


  }
  return(data)






}






