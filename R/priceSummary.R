#' 查询核价明细表
#'
#' @param dms_token  口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceSummary_query()
priceSummary_query <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13'
                             ) {
  sql <- paste0("SELECT [FChartNumber] ERP图号
      ,[FNumber] 物料代码
      ,[FPrice]  成本单价
      ,[FUnit]  计量单位
      ,[FDate] 日期
      ,[FDataSource] 数据源
      ,[FBillNo] 单据编号
      ,[FCoefficient] 换算系数
  FROM [dbo].[rds_lc_ods_priceModel]
  where FDataSource = 'DM单卷算'
  order by FBillNo")
  data  =tsda::sql_select2(token = dms_token,sql = sql)
  return(data)

}
