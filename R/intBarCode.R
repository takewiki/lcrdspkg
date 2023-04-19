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

#' 针对条码功能进行同步
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' barcode_sync_action()
barcodeInner_sync_action <- function(conn=tsda::conn_rds('LCERP')) {
  sql_sync <- paste0("insert  into t_rds_barcode_sync
 select    a.FBarcode,
case isnull(a.DDXXBZ,'1') when '1' then ''  when '.' then '' when '。' then '' else a.DDXXBZ end as FNote_ERP,
i.FNumber,i.FChartNumber ,m.FBillNo,
a.FCreateDate,
0 as FStatus

from T_SUF_Barcode a
inner join t_icitem i
on a.fitemid = i.FItemID
inner join icmo m
on a.FRelateBillID = m.FInterID
where a.FRelateBillType =85
and FCreateDate >
(select  max(FCreateDate) from t_rds_barcode_sync)")
  tsda::sql_update(conn,sql_sync)
  #更新已经禁用的条码
  sql_deleted <- paste0("update a  set a.FStatus =1 from  t_rds_barcode_sync  a
 inner join rds_barcode_banned_deleted  d
 on a.fbarcode = d.FBarcode
 where a.FStatus =0 ")
  tsda::sql_update(conn,sql_deleted)
  #更新已经匹配的代码
  sql_allocated <- paste0(" update a  set a.FStatus =2 from  t_rds_barcode_sync  a
 inner join takewiki_barcode_allocate_auto  u
 on a.fbarcode = u.FBarcode_inner
 where a.FStatus =0 ")
  tsda::sql_update(conn,sql_allocated)


}




#' 查询未分配的记录
#'
#' @param conn  连接
#' @param FChartNo 图号
#' @param FNote_ERP ERP备注
#' @param n 返回数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' barcodeInner_getUnAllocated()
barcodeInner_getUnAllocated <- function(conn=tsda::conn_rds('LCERP'),
                                        FChartNo ='P203042B143G02',
                                        FNote_ERP='',
                                        n= 10) {

  sql<- paste0(" select  top ",n,"  FChartNumber,FBarcode as FBarcode_inner,FNote_ERP     from t_rds_barcode_sync
 where fstatus = 0  and FChartNumber ='",FChartNo,"' and FNote_ERP='",FNote_ERP,"'
 order by  FChartNumber,FNote_ERP,FBarcode")
  res<- tsda::sql_select(conn,sql)
  return(res)

}





