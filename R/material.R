#' 返回物料数据同步
#'
#' @param erp_token erp口令
#' @param dms_token DMS口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' material_ErpSyncDms()
material_ErpSyncDms <- function(erp_token='BF6CBEB5-BC3E-422B-B96C-32BDACCE32ED',
                                     dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13') {

  sql <-paste0("SELECT [FNumber]
      ,[FChartNumber]
      ,[FName]
      ,[FModel]
      ,[FItemProp]
      ,[FUnitName]
  FROM [dbo].[rds_lc_vw_icitem]")
  data = tsda::sql_select2(token = erp_token,sql = sql)
  ncount =nrow(data)
  if(ncount>0){
    #针对数据进行分页处理
    page_info = tsdo::paging_setting(volume = ncount,each_page = 500)
    npage = nrow(page_info)
    lapply(1:npage, function(page){
      start = page_info$FStart[page]
      end =   page_info$FEnd[page]
      item = data[start:end, ]
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_lc_t_icitemInput',r_object = item,append = T)

    })
    #插入备份数据
    sql_bak <- paste0("insert into  rds_lc_t_icitemBak
select * from rds_lc_t_icitem
where fnumber in
(select fnumber from  rds_lc_t_icitemInput)")
    tsda::sql_insert2(token = dms_token,sql_str = sql_bak)
    #删除已经存旧的数据
    sql_del <- paste0("delete  from rds_lc_t_icitem
where fnumber in
(select fnumber from  rds_lc_t_icitemInput)")
    tsda::sql_delete2(token = dms_token,sql_str = sql_del)
    #插入更新的数据
    sql_ins <- paste0("insert into rds_lc_t_icitem
select  *  from  rds_lc_t_icitemInput")
    tsda::sql_insert2(token = dms_token,sql_str = sql_ins)
    #清空临时input表
    sql_input <- paste0("truncate table rds_lc_t_icitemInput")
    tsda::sql_update2(token = dms_token,sql_str = sql_input)
  }


  return(data)

}
