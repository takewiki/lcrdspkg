#' 查询核价明细表
#'
#' @param dms_token  口令
#' @param FDmNo DM单
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceDetail_query()
priceDetail_query <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                              FDmNo='DMP235000B1561502') {
  sql <- paste0("SELECT   [FDmNo]  DM单号
      ,[FLevel] 级次
      ,[FParentNo] 行号
      ,[FParentQty] 父项物料数量
      ,[FchartNo2] 主图号
      ,[FParamG2] 父项G番
      ,[FParamL2] 父项L番
      ,[FParentItemName] 父项物料名称
      ,[FSubChartNo2] 子项图号
      ,[FSubGNo2] 子项G番
      ,[FSubLNo2] 子项L番
      ,[FSubItemName] 子项物料名称
      ,[FSubItemModel] 子项规格型号
      ,[FNote]   备注
      ,[FIndexTxt] 序号
      ,[FQty] 子项数量
      ,[FLength] 长度
      ,[FTotalQty]  总长度
      ,[FParentChartNumber_ERP] 父项ERP图号
      ,[FSubChartNumber_ERP] 子项ERP图号
      ,[FParentItemNumber] 父项ERP物料代码
      ,[FParentItemProp] 父项物料属性
      ,[FParentUnit] 父项计量单位
      ,[FSubItemNumber] 子项物料编号
      ,[FSubItemProp]  子项物料属性
      ,[FSubUnit] 子项计量单位
      ,[FSubItemNumber_FP] 采购发票料号
      ,[FSubPurchasePrice] 物料成本单价
      ,[FSubUnit_FP]  采购发票单位
      ,[FSubPurchaseDate] 单据日期
      ,[FDataSource] 数据源
      ,[FBillNo] 单据编号
      ,[FCoefficient] 换算系数
      ,[FSubAmt] 子项成本金额
      ,[FParentAmt] 父项成本金额
  FROM [dbo].[rds_lc_ods_priceModelDetail]
where FDmNo='",FDmNo,"'
order by fparentNo")
  data  =tsda::sql_select2(token = dms_token,sql = sql)
  return(data)

}
