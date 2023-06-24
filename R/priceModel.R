#' 获取DM单的所有的单据编号
#'
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_getBillNo()
dmList_getBillNo<- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13'){
  sql <- paste0("SELECT
distinct [FDmNo]
  FROM [dbo].[rds_lc_ODS_dmList]  where FIsDo = 0")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  ncount =nrow(data)
  if(ncount>0){
    res <- data$FDmNo
  }else{
    res <- NA
  }
  return(res)
}


#' 获取DM单的所有的单据编号
#'
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_getBillNo()
dmList_getBillNo2<- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13'){
  sql <- paste0("select distinct  FDmNo  from t_lcrds_dmlist
where FDmNo not in
(
select distinct FDmNo from rds_lc_ODS_dmList

) ")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  ncount =nrow(data)
  if(ncount>0){
    res <- data$FDmNo
  }else{
    res <- NA
  }
  return(res)
}



#' 判断是否存在新的DM单
#'
#' @param dms_token 口令
#' @param FDmNo DM单
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmListODS_isNew()
dmListODS_isNew <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                         FDmNo='DMP235000B1561502') {
  sql <- paste0("select 1 from rds_lc_ODS_dmList
where FDmNo ='",FDmNo,"'")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  ncount = nrow(data)
  if(ncount>0){
    res <- FALSE
  }else{
    res <- TRUE
  }
  return(res)
}


#' 查看清单的数据情况
#'
#' @param dms_token 中台口令
#' @param FDmNo  DM单号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_toODS()
dmList_toODS <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                         FDmNo='DMP235000B1561502') {
  sql <- paste0("SELECT [FDmNo]
      ,[FLevel]
      ,[FParentNo]
      ,[FParentQty]
      ,[FchartNo2]
      ,[FParamG2]
      ,[FParamL2]
      ,[FParentItemName]
      ,[FSubChartNo2]
      ,[FSubGNo2]
      ,[FSubLNo2]
      ,[FSubItemName]
      ,[FSubItemModel]
      ,[FNote]
      ,[FIndexTxt]
      ,[FQty]
      ,[FLength]
      ,[FTotalQty]
  FROM [dbo].[rds_lc_vw_dmList]
  where FDmNo='",FDmNo,"'")
  data =tsda::sql_select2(token = dms_token,sql = sql)
  ncount =nrow(data)
  if(ncount>0){

    #如果数据已经存在，则不再写入
    if(dmListODS_isNew(dms_token = dms_token,FDmNo = FDmNo)){
      #全新的DM单
      #存在记录的情况下
      #针对父项物料，进行接接成标准化物料,功能相当于ERP系统中的图号字段
      #ERP系统中的图号字段由主图号/分图号+G番+L番组成；另外一种情况是直接由DM单号组成；
      data$FParentChartNumber_ERP <-paste0(data$FchartNo2,Gtab_standard(data$FParamG2),Ltab_standard(data$FParamL2))
      data$FSubChartNumber_ERP <- paste0(data$FSubChartNo2,Gtab_standard(data$FSubGNo2),Ltab_standard(data$FSubLNo2))
      #针对DM单进行处理
      data$FParentChartNumber_ERP[data$FLevel == 0] <- data$FDmNo[data$FLevel == 0]
      # 针对数据增加状态处理
      data$FIsDo = 0
      #将数据写入数据库
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_lc_ODS_dmList',r_object = data,append = TRUE)
    }



  }
  #不能返回数据，否则数据量太大，影响相关性能
  #后续还是应该将数据读取与写入数据库分开为2个功能

  res <- TRUE
  return(res)

}



#' 创建DM单ODS表中创建表结构
#'
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmListOds_createTable()
dmListOds_createTable <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13') {
  sql <-paste0("CREATE TABLE [dbo].[rds_lc_ODS_dmList](
	[FDmNo] [varchar](50) NULL,
	[FLevel] [int] NULL,
	[FParentNo] [varchar](200) NOT NULL,
	[FParentQty] [numeric](16, 3) NULL,
	[FchartNo2] [varchar](100) NULL,
	[FParamG2] [varchar](50) NULL,
	[FParamL2] [varchar](50) NOT NULL,
	[FParentItemName] [varchar](500) NULL,
	[FSubChartNo2] [varchar](100) NULL,
	[FSubGNo2] [varchar](50) NULL,
	[FSubLNo2] [varchar](50) NULL,
	[FSubItemName] [varchar](500) NULL,
	[FSubItemModel] [varchar](500) NULL,
	[FNote] [varchar](500) NULL,
	[FIndexTxt] [varchar](50) NULL,
	[FQty] [numeric](16, 3) NULL,
	[FLength] [numeric](16, 3) NULL,
	[FTotalQty] [numeric](16, 3) NULL,
	FParentChartNumber_ERP  [varchar](100) NULL,
	FSubChartNumber_ERP  [varchar](100) NULL

) ")
  tsda::sql_update2(token = dms_token,sql_str = sql)

}


#' 查看清单的数据情况
#'
#' @param dms_token 中台口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_toODSBatch()
dmList_toODSBatch <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13'){
  DmNos = dmList_getBillNo2(dms_token = dms_token)
  ncount = length(DmNos)
  lapply(1:ncount,function(i){
    FDmNo = DmNos[i]
    print(paste0('正在处理第',i,'个DM单,总数',ncount,"个DM单,当前单据编号为",FDmNo))
    dmList_toODS(dms_token = dms_token,FDmNo = FDmNo)
  })
}

#' 获取DM最大的级次
#'
#' @param dms_token 口令
#' @param FDmNo DM单号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_LevelMax()
dmList_LevelMax <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                            FDmNo='DMP235000B1561920') {

  sql <-paste0("select   max(FLevel) as FLevelMax from   [rds_lc_ODS_dmList]
where FDmNo='",FDmNo,"' ")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  res = data$FLevelMax
  return(res)

}


#' 获取的某一级次的行号清单
#'
#' @param dms_token 口令
#' @param FDmNo DM单号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dmList_ParentNo()
dmList_ParentNo <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                            FDmNo='DMP235000B1561920'
                         ) {

  sql <-paste0("select  distinct  FDmNo,FLevel,FParentNo  from   [rds_lc_ODS_dmList]
where FDmNo='",FDmNo,"'
order by FLevel desc,FParentNo desc")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  return(data)

}

#' 查询数据
#'
#' @param dms_token 口令
#' @param FDmNo  DM单号
#' @param FLevel 级次
#' @param FParentNo 行号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_query()
priceModel_query <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                                 FDmNo='DMP235000B1561920',
                                 FLevel =2,
                                 FParentNo='0080020'
){
  sql <- paste0("select    [FDmNo]
      ,[FLevel]
      ,[FParentNo]
      ,[FParentQty]
      ,[FchartNo2]
      ,[FParamG2]
      ,[FParamL2]
      ,[FParentItemName]
      ,[FSubChartNo2]
      ,[FSubGNo2]
      ,[FSubLNo2]
      ,[FSubItemName]
      ,[FSubItemModel]
      ,[FNote]
      ,[FIndexTxt]
      ,[FQty]
      ,[FLength]
      ,[FTotalQty]
      ,[FParentChartNumber_ERP]
      ,[FSubChartNumber_ERP]
      from   [rds_lc_ODS_dmList]
where FDmNo='",FDmNo,"' and FLevel = ",FLevel,"
and FParentNo ='",FParentNo,"'
order by FIndexTxt")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  return(data)


}


#' 获取物料检查信息
#'
#' @param dms_token 口令
#' @param FChartNumber 图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_getItemOne()
priceModel_getItemOne <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                 FChartNumber ='X39DM-230'
                 ) {
  sql <-paste0("select top 1 FChartNumber,FNumber,FItemProp,FUnitName from rds_lc_t_icitem
where  FChartNumber ='",FChartNumber,"'")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  ncount = nrow(data)
  if(ncount>0){
    res = data
  }else{
    res = data.frame(FChartNumber=FChartNumber,FNumber="图号对应的物料不存在",FItemProp="",FUnitName="")
  }
  return(res)
}

#' 获取物料检查信息
#'
#' @param FChartNumbers 图号
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_getItemAll()
priceModel_getItemAll <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                                  FChartNumbers =c('X39DM-230',
                                                   'X39DM-204',
                                                   'X39BX-43',
                                                   'X39BX-01',
                                                   'Z148B0.30-WH-'
                                  )
) {
  res_split = lapply(FChartNumbers, function(FChartNumber){
    priceModel_getItemOne(dms_token = dms_token,FChartNumber = FChartNumber)
  })
  res = do.call('rbind',res_split)
  return(res)
}


#' 获取物料检查信息
#'
#' @param dms_token 口令
#' @param FChartNumber 图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_getPriceOne()
priceModel_getPriceOne <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                                  FChartNumber ='X39DM-230'
) {
  sql <-paste0("select   top 1 FChartNumber,FNumber,FPrice,FUnit,FDate,FDataSource,FBillNo,FCoefficient from  rds_lc_ods_priceModel
where  FChartNumber ='",FChartNumber,"'")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  ncount = nrow(data)
  if(ncount>0){
    res = data
  }else{
    res = data.frame(FChartNumber=FChartNumber,FNumber="图号对应的物料不存在",FPrice=0,FUnit="",FDate=tsdo::getDate(),FDataSource='DMS数据中台',FBillNo='',FCoefficient=1)
  }
  return(res)
}


#' 获取物料检查信息
#'
#' @param FChartNumbers 图号
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_getPriceAll()
priceModel_getPriceAll <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                                  FChartNumbers =c('X39DM-230',
                                                   'X39DM-204',
                                                   'X39BX-43',
                                                   'X39BX-01',
                                                   'Z148B0.30-WH-'
                                  )
) {
  res_split = lapply(FChartNumbers, function(FChartNumber){
    priceModel_getPriceOne(dms_token = dms_token,FChartNumber = FChartNumber)
  })
  res = do.call('rbind',res_split)
  return(res)
}

#' 价格模型创建表结构
#'
#' @param dms_token 中台口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_createTable()
priceModel_createTable <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13') {
  sql <- paste0("CREATE TABLE [dbo].[rds_lc_ods_priceModel](
	[FChartNumber] [nvarchar](255) NULL,
	[FNumber] [varchar](80) NULL,
	[FPrice] [decimal](36, 10) NULL,
	[FUnit] [nvarchar](80) NULL,
	[FDate] [datetime] NULL,
	[FDataSource] [varchar](10) NOT NULL,
	[FBillNo] [nvarchar](255) NOT NULL,
	[FCoefficient] [decimal](28, 10) NULL
)")
  tsda::sql_update2(token = dms_token,sql_str = sql)

}


#' 价格模型创建表结构
#'
#' @param dms_token 中台口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_createTable()
priceModelDetail_createTable <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13') {
  sql <- paste0("create table  rds_lc_ods_priceModelDetail
(
FDmNo varchar(50),
FLevel int,
FParentNo varchar(200),
FParentQty  decimal(16,3),
FchartNo2 varchar(100),
FParamG2 varchar(50),
FParamL2 varchar(50),
FParentItemName varchar(500),
FSubChartNo2 varchar(100),
FSubGNo2 varchar(50),
FSubLNo2 varchar(50),
FSubItemName varchar(500),
FSubItemModel varchar(500) ,
FNote  varchar(500),
FIndexTxt varchar(50),
FQty decimal(16,3),
FLength  decimal(16,3),
FTotalQty  decimal(16,3),
FParentChartNumber_ERP varchar(100),
FSubChartNumber_ERP  varchar(100),
FParentItemNumber varchar(80),
FParentItemProp nvarchar(100),
FParentUnit varchar(80),
FSubItemNumber varchar(80),
FSubItemProp nvarchar(100),
FSubUnit varchar(80),
FSubItemNumber_FP varchar(80),
FSubPurchasePrice  decimal(36,10),
FSubUnit_FP  nvarchar(160),
FSubPurchaseDate datetime,
FDataSource varchar(50),
FBillNo nvarchar(510),
FCoefficient decimal(28,10),
FSubAmt decimal(28,10),
FParentAmt decimal(28,10)
)")
  tsda::sql_update2(token = dms_token,sql_str = sql)

}


#' 价格模型创建表结构
#'
#' @param dms_token 中台口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_purchaseToDMS()
priceModel_purchaseToDMS <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13') {
  sql <- paste0("insert into rds_lc_ods_priceModel
select   FChartNumber,FNumber,FPrice,FUnit,FDate,FDataSource,FBillNo,FCoefficient
from  rds_lc_vw_purchasePriceAll")
  tsda::sql_update2(token = dms_token,sql_str = sql)

}


#' 查询数据
#'
#' @param dms_token 口令
#' @param FDmNo  DM单号
#' @param FLevel 级次
#' @param FParentNo 行号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_Unit()
priceModel_Unit <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                             FDmNo='DMP235000B1561920',
                             FLevel =2,
                             FParentNo='0080020'
){
  options(scipen = 9)
  if(FLevel >=1){
    #读取数据
    data = priceModel_query(dms_token = dms_token,FDmNo = FDmNo,FLevel = FLevel,FParentNo = FParentNo)
    #父项物料信息
    Parentitem_Info = priceModel_getItemAll(dms_token = dms_token,FChartNumbers = data$FParentChartNumber_ERP)
    data$FParentItemNumber = Parentitem_Info$FNumber
    data$FParentItemProp =  Parentitem_Info$FItemProp
    data$FParentUnit <- Parentitem_Info$FUnitName
    #子项物料信息
    Subitem_Info = priceModel_getItemAll(dms_token = dms_token,FChartNumbers = data$FSubChartNumber_ERP)
    data$FSubItemNumber = Subitem_Info$FNumber
    data$FSubItemProp =  Subitem_Info$FItemProp
    data$FSubUnit <- Subitem_Info$FUnitName
    #单价信息
    price_Info = priceModel_getPriceAll(dms_token = dms_token,FChartNumbers = data$FSubChartNumber_ERP)
    data$FSubItemNumber_FP <- price_Info$FNumber
    data$FSubPurchasePrice <- price_Info$FPrice
    data$FSubUnit_FP <- price_Info$FUnit
    data$FSubPurchaseDate <- price_Info$FDate
    data$FDataSource <- price_Info$FDataSource
    data$FBillNo <- price_Info$FBillNo
    data$FCoefficient <- price_Info$FCoefficient
    #将采购单位换算成基本单位
    #还是没有考虑到BOM的的单位，如线索的单位到底是多少
    data$FSubAmt = data$FTotalQty * data$FSubPurchasePrice / data$FCoefficient
    data$FParentAmt = sum(data$FSubAmt)
  }else{
    #针对顶级的数据处理Level=0
    data = priceModel_query(dms_token = dms_token,FDmNo = FDmNo,FLevel = FLevel,FParentNo = FParentNo)
    #父项物料信息
    Parentitem_Info = priceModel_getItemAll(dms_token = dms_token,FChartNumbers = data$FParentChartNumber_ERP)
    data$FParentItemNumber = Parentitem_Info$FNumber
    data$FParentItemProp =  Parentitem_Info$FItemProp
    data$FParentUnit <- Parentitem_Info$FUnitName
    #子项信息
    data$FSubItemNumber = ''
    data$FSubItemProp =  ''
    data$FSubUnit <- ''
    #单价信息
    data$FSubItemNumber_FP <- ''
    data$FSubPurchasePrice <- 0
    data$FSubUnit_FP <- ''
    data$FSubPurchaseDate <- tsdo::getDate()
    data$FDataSource <- 'DM单卷算'
    data$FBillNo <- FDmNo
    data$FCoefficient <- 1
    data$FSubAmt = 1
    sql_totalAmt <-paste0("select sum(FParentAmt) as FParentAmt from rds_lc_ods_priceModelLevel1
where FDmNo='",FDmNo,"'")
    data_totalAmt =tsda::sql_select2(token = dms_token,sql = sql_totalAmt)
    data$FParentAmt = data_totalAmt$FParentAmt
  }
  #将过程表数据写入模型明细表
  tsda::db_writeTable2(token = dms_token,table_name = 'rds_lc_ods_priceModelDetail',r_object = data,append = TRUE)
  model = data.frame(FChartNumber=data$FParentChartNumber_ERP[1],
                     FNumber = data$FParentItemNumber[1],
                     FPrice = data$FParentAmt[1],
                     FUnit =data$FParentUnit[1],
                     FDate = tsdo::getDate(),
                     FDataSource = 'DM单卷算',
                     FBillNo = data$FDmNo[1],
                     FCoefficient = 1,
                     stringsAsFactors = FALSE
  )
  #将模型数据写入价格模板
  tsda::db_writeTable2(token = dms_token,table_name = 'rds_lc_ods_priceModel',r_object = model,append = TRUE)

 # return(data)

}


#' 更新DM单状态
#'
#' @param dms_token 口令
#' @param FDmNo DM单号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_DmStatus()
priceModel_DmStatus <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                                 FDmNo='DMP235000B1561920'
){
  sql <- paste0("    update rds_lc_ODS_dmList  set FIsDo = 1
  where FDmNo  ='",FDmNo,"'")
  tsda::sql_update2(token = dms_token,sql_str = sql)
}


#' DM异常处理，删除只有Level等于0的记录
#'
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_DmErrorDel()
priceModel_DmErrorDel <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13'

){
  sql_src <- paste0("   delete    FROM [dbo].[t_lcrds_dmlist]
  where FDmNo   in (  select fdmno from t_lcrds_dmlist
  group by  fdmno
  having max(flevel) =0)")
  tsda::sql_update2(token = dms_token,sql_str = sql_src)
  sql_ods <- paste0("   delete    FROM [dbo].[rds_lc_ODS_dmList]
  where FDmNo    in (  select fdmno from rds_lc_ODS_dmList
  group by  fdmno
  having max(flevel) =0)")
  tsda::sql_update2(token = dms_token,sql_str = sql_ods)

}


#' 处理一条记录
#'
#' @param dms_token 中台口令
#' @param FDmNo 单号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_DmCalcOne()
priceModel_DmCalcOne <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13',
                            FDmNo='DMP235000B1561920'

){

  dmBill = dmList_ParentNo(dms_token = dms_token,FDmNo = FDmNo)
  ncount = nrow(dmBill)
  if(ncount>0){
    lapply(1:ncount,function(i){

      FDmNo = dmBill$FDmNo[i]
      FLevel = dmBill$FLevel[i]
      FParentNo = dmBill$FParentNo[i]
      priceModel_Unit(dms_token = dms_token,FDmNo = FDmNo,FLevel = FLevel,FParentNo = FParentNo)


    })
    #处理完了更新一下数据状态
    priceModel_DmStatus(dms_token = dms_token,FDmNo = FDmNo)
  }

}



#' 处理所有数据
#'
#' @param dms_token 中台口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' priceModel_DmCalcAll()
priceModel_DmCalcAll <- function(dms_token='048017E3-CA7E-4DC7-BC87-0EA7D8C69C13'


){
  #处理只有0级的DM单异常数据
  priceModel_DmErrorDel(dms_token = dms_token)
  #获取所有的单据编号
  DmBills = dmList_getBillNo(dms_token = dms_token)
  #按DM单处理每个单号，完成BOM成本卷算
  lapply(DmBills, function(FDmNo){
    print(FDmNo)
    priceModel_DmCalcOne(dms_token = dms_token,FDmNo = FDmNo)
  })
}


