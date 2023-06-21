#' 返回采购单价查询
#'
#' @param erp_token ERP口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchasePrice_query()
purchasePriceErp_query <- function(erp_token='BF6CBEB5-BC3E-422B-B96C-32BDACCE32ED') {

  sql <-paste0("SELECT
       [FNumber]
      ,[FMaterialName]
      ,[FModel]
      ,[FChartNumber]
      ,[FSupplierNumber]
      ,[FSupplierName]
      ,[FCurrencyNumber]
      ,[FCurrencyName]
      ,[FInterID]
      ,[FBillNo]
      ,[FDate]
      ,[FPrice]
  FROM  [rds_lc_vw_material_purchasePrice_latest]")
  data = tsda::sql_select2(token = erp_token,sql = sql)
  return(data)

}

#' 返回采购单价数据同步
#'
#' @param erp_token erp口令
#' @param dms_token DMS口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchasePriceLast_syncRds()
purchasePrice_ErpSyncDms <- function(erp_token='BF6CBEB5-BC3E-422B-B96C-32BDACCE32ED',
                                      dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13') {

  sql <-paste0("SELECT
       [FNumber]
      ,[FMaterialName]
      ,[FModel]
      ,[FChartNumber]
      ,[FSupplierNumber]
      ,[FSupplierName]
      ,[FCurrencyNumber]
      ,[FCurrencyName]
      ,[FInterID]
      ,[FBillNo]
      ,[FDate]
      ,[FPrice]
  FROM  [rds_lc_vw_material_purchasePrice_latest]")
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
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_lc_t_material_purchasePrice_latestInput',r_object = item,append = T)

    })
    #插入备份数据
    sql_bak <- paste0("insert into  rds_lc_t_material_purchasePrice_latestBak
select * from rds_lc_t_material_purchasePrice_latest
where fnumber in
(select fnumber from  rds_lc_t_material_purchasePrice_latestInput)")
    tsda::sql_insert2(token = dms_token,sql_str = sql_bak)
    #删除已经存旧的数据
    sql_del <- paste0("delete  from rds_lc_t_material_purchasePrice_latest
where fnumber in
(select fnumber from  rds_lc_t_material_purchasePrice_latestInput)")
    tsda::sql_delete2(token = dms_token,sql_str = sql_del)
    #插入更新的数据
    sql_ins <- paste0("insert into rds_lc_t_material_purchasePrice_latest
select  *  from  rds_lc_t_material_purchasePrice_latestInput")
    tsda::sql_insert2(token = dms_token,sql_str = sql_ins)
    #清空临时input表
    sql_input <- paste0("truncate table rds_lc_t_material_purchasePrice_latestInput")
    tsda::sql_update2(token = dms_token,sql_str = sql_input)
  }


  return(data)

}






#' 创建表结构
#'
#' @param dms_token DMS口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchasePriceLast_createTable()
purchasePriceDms_createTable <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13') {
  #创建input表
 sql_input <- paste0("CREATE TABLE [dbo].[rds_lc_t_material_purchasePrice_latestInput](
	[FNumber] [varchar](80) NULL,
	[FMaterialName] [varchar](80) NULL,
	[FModel] [varchar](255) NULL,
	[FChartNumber] [varchar](255) NULL,
	[FSupplierNumber] [varchar](255) NULL,
	[FSupplierName] [varchar](80) NULL,
	[FCurrencyNumber] [varchar](3) NULL,
	[FCurrencyName] [varchar](40) NULL,
	[FInterID] [int] NOT NULL,
	[FBillNo] [nvarchar](255) NOT NULL,
	[FDate] [datetime] NULL,
	[FPrice] [decimal](28, 10) NOT NULL
)")
 sql <- paste0("CREATE TABLE [dbo].[rds_lc_t_material_purchasePrice_latest](
	[FNumber] [varchar](80) NULL,
	[FMaterialName] [varchar](80) NULL,
	[FModel] [varchar](255) NULL,
	[FChartNumber] [varchar](255) NULL,
	[FSupplierNumber] [varchar](255) NULL,
	[FSupplierName] [varchar](80) NULL,
	[FCurrencyNumber] [varchar](3) NULL,
	[FCurrencyName] [varchar](40) NULL,
	[FInterID] [int] NOT NULL,
	[FBillNo] [nvarchar](255) NOT NULL,
	[FDate] [datetime] NULL,
	[FPrice] [decimal](28, 10) NOT NULL
)")

 sql_bak <- paste0("CREATE TABLE [dbo].[rds_lc_t_material_purchasePrice_latestBak](
	[FNumber] [varchar](80) NULL,
	[FMaterialName] [varchar](80) NULL,
	[FModel] [varchar](255) NULL,
	[FChartNumber] [varchar](255) NULL,
	[FSupplierNumber] [varchar](255) NULL,
	[FSupplierName] [varchar](80) NULL,
	[FCurrencyNumber] [varchar](3) NULL,
	[FCurrencyName] [varchar](40) NULL,
	[FInterID] [int] NOT NULL,
	[FBillNo] [nvarchar](255) NOT NULL,
	[FDate] [datetime] NULL,
	[FPrice] [decimal](28, 10) NOT NULL
)")
 #创建表
 tsda::sql_update2(token = dms_token,sql_str = sql_input)
 tsda::sql_update2(token = dms_token,sql_str = sql)
 tsda::sql_update2(token = dms_token,sql_str = sql_bak)




}




#' 采购价格最新数据查询
#'
#' @param dms_token  dms口令
#' @param FChartNumber 图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchsePriceLastRds_query()
purchsePriceDms_query <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                                      FChartNumber =''
                                      ) {
  if (FChartNumber==''){
    sql <- paste0("select
FNumber  物料编码
, FMaterialName  物料名称
,isnull(FModel,'')   规格型号
,isnull(FChartNumber,'')  图号
,FPrice 采购单价
, FBillNo  发票号
,FDate 发票日期
,FCurrencyName 币别
from rds_lc_t_material_purchasePrice_latest")
  }else{
    sql <- paste0("select
FNumber  物料编码
, FMaterialName  物料名称
,isnull(FModel,'')   规格型号
,isnull(FChartNumber,'')  图号
,FPrice 采购单价
, FBillNo  发票号
,FDate 发票日期
,FCurrencyName 币别
from rds_lc_t_material_purchasePrice_latest
where FChartNumber ='",FChartNumber,"'")
  }
  data = tsda::sql_select2(token = dms_token,sql = sql)
  return(data)



}
