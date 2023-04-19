#按图号+G番+L番删除对应的BOM------
#' 针对数据进行处理
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FGtab G
#' @param FLtab L
#'
#' @return return value
#' @export
#'
#' @examples
#' dm_bom_deleteDB()
dm_bom_deleteDB <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672',FGtab ='G10',FLtab ='L02') {
  #备注数据
  sql_bak <-paste0("insert into t_lcrds_bomDel
select * from t_lcrds_bom  where FchartNo='",FchartNo,"' and FParamG='",FGtab,"' and FParamL='",FLtab,"'")
  tsda::sql_update(conn,sql_bak)

  sql_del <- paste0("delete from t_lcrds_bom  where FchartNo='",FchartNo,"' and FParamG='",FGtab,"' and FParamL='",FLtab,"'")
  tsda::sql_update(conn,sql_del)

}

#按图号删除BOM--------
#' 按主图号删除数据库
#'
#' @param conn  连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_chartNo_deleteDB()
dm_chartNo_deleteDB <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672') {
  #备注数据
  sql_bak <-paste0("insert into t_lcrds_bomDel
select * from t_lcrds_bom  where FchartNo='",FchartNo,"'")
  tsda::sql_update(conn,sql_bak)

  sql_del <- paste0("delete from t_lcrds_bom  where FchartNo='",FchartNo,"'")
  tsda::sql_update(conn,sql_del)

}

#按图号删除BOM--------
#' 按主图号删除数据库,按番与L番进行处理
#'
#' @param conn  连接
#' @param FchartNo 主图号
#' @param FParamG  G番
#' @param FParamL L番
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_chartNo_delete_byGL()
dm_chartNo_delete_byGL <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672',FParamG = 'G01',FParamL ='L57') {
  #备注数据
  sql_bak <-paste0("insert into t_lcrds_bomDel
select * from t_lcrds_bom where fchartNo ='",FchartNo,"' and FParamG = '",FParamG,"' and FParamL ='",FParamL,"'")
  tsda::sql_update(conn,sql_bak)

  sql_del <- paste0("delete from t_lcrds_bom where fchartNo ='",FchartNo,"' and FParamG = '",FParamG,"' and FParamL ='",FParamL,"'")
  tsda::sql_update(conn,sql_del)

}








#单级BOM的核心逻辑函数------
#' 更新记录
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FLtab L番
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_ReadBy_ChartNo_Ltab()
dm_ReadBy_ChartNo_Ltab <- function(conn=tsda::conn_rds('lcrds'),FchartNo='YX200A714',FLtab ='L07') {

  #读取基础表,减少对G的依赖
  #所有的G表一起处理
  sql <- paste0("select FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FGtab  from t_lcrds_gtab where FchartNo ='",FchartNo,"'")
  #读取数据,其中r表示G表
  r <- tsda::sql_select(conn,sql)
  # print('1')
  # print(r[r$FIndexTxt =='-13',])
  ncount <- nrow(r)
  if(ncount>0){
    #针对整体数据进行处理
    r$FLength <- 1
    for (i in 1:ncount) {

      # if(r[i,'FIndexTxt'] =='-13'){
      #   print('2')
      #   print(r[i,'FIndexTxt'])
      # }


      #针对每一行数据进行处理,
      #针对件号进行处理
      keyNo <- tsdo::na_replace(r[i,'FkeyNo'],'')
      #仅仅针对变量做了处理，所以不对的
      #针对件号中的变量进行处理

        #获取所有的变量变量
        #vars <-Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
        keyNo_type = bom_getKeyNoType(conn=conn,FchartNo = FchartNo,keyNo = keyNo)
        if(keyNo_type =='var'){
          #如果结果是变理，需要查询L番表
          length_value <- Ltab_get_varValue(conn = conn,FchartNo = FchartNo,FLtab = FLtab,FkeyNo = keyNo)
          value_type = bom_getVarValueType(length_value)
          #针对结果进行2次处理
          if(value_type =='G'){
            #G番,替代相应的件号
            r[i,'FkeyNo'] <- length_value

          }
          if(value_type == 'seq'){
            #序号，替代相应的件号
            r[i,'FkeyNo'] <- length_value
          }
          if(value_type=='length'){
            #设置相应的长度
            #这一部分的逻辑不对，目前仅仅是对G番进行判断，为什么要去设置数量变量
                  length_value <- as.numeric(length_value)
                  #针对值进行处理
                  r[i,'FLength'] <- length_value

          }else{
            #设置相应的长度为0
            #bug
            #如果件号没有的话也不应该设置为0
            #不做处理即可
            #r[i,'FLength'] <- 0
          }


        }

        if(keyNo_type =='seq'){
          #如果结果是序号,则保持不变

        }
        if(keyNo_type =='G'){
          #如果结果是G番,则保持不变

        }

        if(keyNo_type =='NA'){
          #如果结果是NA,则变质不变

        }
        if(keyNo_type =='fixed'){
          #如果结果是常量,则需要设置Flength
          #设置的结果还是不对，目前仅仅是针对G番进行判断
          r[i,'FLength'] <- as.numeric(keyNo)

        }
        # if( keyNo %in% vars){
        #   #需要处理
        #   length_value <- Ltab_get_varValue(conn = conn,FchartNo = FchartNo,FLtab = FLtab,FkeyNo = keyNo)
        #   #针对表取数来后，分4种情况情况
        #
        #   if(is.numeric(as.numeric(length_value)) &(!is.na(as.numeric(length_value)))){
        #     #情况1 长度替代
        #     #print(as.numeric(length_value))
        #     #print(as.numeric(length_value)>0)
        #     if(as.numeric(length_value) >0){
        #       #真的长度
        #       length_value <- as.numeric(length_value)
        #       #针对值进行处理
        #       r[i,'FLength'] <- length_value
        #     }else{
        #       #还是序号
        #       if(as.numeric(length_value) <0){
        #         r[i,'FkeyNo'] <- length_value
        #         print('1')
        #       }
        #
        #     }
        #
        #
        #   }else{
        #     #情况2 G番替代或者文本替代
        #     #针对件号进行处理
        #     r[i,'FkeyNo'] <- length_value
        #
        #   }
        #
        #
        #
        #
        #
        #
        #
        #
        # }



      #针对件号中的常数进行处理
      # if(is.numeric(as.numeric(keyNo))){
      #   r[i,'FLength'] <- as.numeric(keyNo)
      #
      # }
        # if(r[i,'FIndexTxt'] =='-13'){
        #   print('3')
        #   print(r[i,'FIndexTxt'])
        # }


      #针对L番进行处理
      ltab <-tsdo::na_replace(r[i,'FLtab'],'')
      if(tsdo::len(ltab)){
        vars <-Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
        if( ltab %in% vars){
          #针对数据处理处理
          length_value <- Ltab_get_varValue(conn = conn,FchartNo = FchartNo,FLtab = FLtab,FkeyNo = ltab)
          #print(length_value)
          r[i,'FLtab'] <- length_value



        }

      }
      # if(r[i,'FIndexTxt'] =='-13'){
      #   print('4')
      #   print(r[i,'FIndexTxt'])
      # }

      #针对数量进行处理
      fqty <- tsdo::na_replace(r[i,'FQty'],"")
      if(tsdo::len(fqty)){
        vars <-Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
        if( fqty %in% vars){
          #针对数据处理处理
          length_value <- Ltab_get_varValue(conn = conn,FchartNo = FchartNo,FLtab = FLtab,FkeyNo = fqty)
          length_value <- as.numeric(length_value)
          r[i,'FQty'] <- length_value



        }

      }
      # if(r[i,'FIndexTxt'] =='-13'){
      #   print('5')
      #   print(r[i,'FIndexTxt'])
      # }





    }
    # print('6')
    # print(r[r$FIndexTxt =='-13',])
    #数据已经处理完了
    r$FQty <- as.numeric(r$FQty)
    r$FLength <- as.numeric(r$FLength)
    # print('7')
    # print(r[r$FIndexTxt =='-13',])
    #针对数据处理处理,其中数量的na给予0处理
    r$FQty <- tsdo::na_replace(r$FQty,0)
    r$FLength <- tsdo::na_replace(r$FLength,0)
    # print('8')
    # print(r[r$FIndexTxt =='-13',])

    r$FTotalQty <- r$FQty * r$FLength
    # print('9')
    # print(r[r$FIndexTxt =='-13',])
    r$FParamG <- r$FGtab
    r$FParamL <- FLtab
    # print('10')
    # print(r[r$FIndexTxt =='-13',])
    #针对空行进行处理,删除空行
    #针对汇总行也进行相应的处理

    # r <- r[!is.na(r$FQty),]
    # r <- r[!is.na(r$FTotalQty),]
     r <- r[r$FQty  > 0 , ]
     r <- r[r$FTotalQty > 0 , ]
     # print('10')
     # print(r[r$FIndexTxt =='-13',])
    #针对列进行处理
    Gtab_colnames <- names(r)
    Gtab_colNames_sel <- !Gtab_colnames %in% 'FGtab'
    # print('11')
    # print(r[r$FIndexTxt =='-13',])
    r <- r[ ,Gtab_colNames_sel]

  }else{
    r<-NA
  }

  return(r)



}


#' BOM查询多L番
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FParamG G番
#' @param FParamL L番
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_selectDB_detail2()
dm_selectDB_detail2 <- function(conn=tsda::conn_rds('lcrds'),FchartNo ='SE304A200', FParamG ='G01'  , FParamL ='') {

  if (FParamL == ''){
    dm_writeDB_ChartNo_G(conn = conn,FchartNo = FchartNo,FParamG = FParamG)
    #针对L番进行判断
    flag =  Ltab_checkExist(conn = conn,FchartNo = FchartNo)
    if(flag){
      #存在L番，留空表示所有L番
      sql = paste0("select FchartNo,
  FParamG,
  FParamL,
  FItemName,
  FSubChartNo,
  FkeyNo,
  FLtab,
  FItemModel,
  FNote,
  FIndexTxt,
  FQty,
  FLength,
  FTotalQty
  from  t_lcrds_bom
  where FchartNo ='",FchartNo,"' and FParamG ='",FParamG,"'
                  order by  FParamG,FParamL, FIndexTxt")

    }else{
      #不存在L番，使用空表示
      sql = paste0("select FchartNo,
  FParamG,
  FParamL,
  FItemName,
  FSubChartNo,
  FkeyNo,
  FLtab,
  FItemModel,
  FNote,
  FIndexTxt,
  FQty,
  FLength,
  FTotalQty
  from  t_lcrds_bom
  where FchartNo ='",FchartNo,"' and FParamG ='",FParamG,"'  and FParamL =  '",FParamL,"'
                 order by  FParamG,FParamL, FIndexTxt")


    }

  }else{
    #重算部分数据
    dm_ReadBy_ChartNo_GL_dealOne(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL = FParamL,page_size = 300)
    sql = paste0("select FchartNo,
  FParamG,
  FParamL,
  FItemName,
  FSubChartNo,
  FkeyNo,
  FLtab,
  FItemModel,
  FNote,
  FIndexTxt,
  FQty,
  FLength,
  FTotalQty
  from  t_lcrds_bom
  where FchartNo ='",FchartNo,"' and FParamG ='",FParamG,"'  and FParamL =  '",FParamL,"'
                 order by  FParamG,FParamL, FIndexTxt")
  }
  print(sql)
  res <- tsda::sql_select(conn,sql)
  #针对数据进行处理
  ncount <- nrow(res)
  if(ncount>0){
    names(res) <-c('主图号','G番号-参数','L番号-参数','子项名称','分图号','子项件号','子项L番','子项规格',
                    '子项备注','子项序号','子项基本数量','子项长度/系数','子项总数量')

   }

    #else{
  #   #进行二次取数
  #   if (FParamL == ''){
  #     #重算所有G番
  #     dm_writeDB_ChartNo_G(conn = conn,FchartNo = FchartNo,FParamG = FParamG)
  #     #重新查询
  #
  #
  #     sql = paste0("select FchartNo,
  # FParamG,
  # FParamL,
  # FItemName,
  # FSubChartNo,
  # FkeyNo,
  # FLtab,
  # FItemModel,
  # FNote,
  # FIndexTxt,
  # FQty,
  # FLength,
  # FTotalQty
  # from  t_lcrds_bom
  # where FchartNo ='",FchartNo,"' and FParamG ='",FParamG,"'
  #                  order by  FParamG,FParamL, FIndexTxt
  #                  ")
  #     res <- tsda::sql_select(conn,sql)
  #     if(nrow(res)>0){
  #       names(res) <-c('主图号','G番号-参数','L番号-参数','子项名称','分图号','子项件号','子项L番','子项规格',
  #                      '子项备注','子项序号','子项基本数量','子项长度/系数','子项总数量')
  #     }
  #
  #   }else{
  #     #重算部分数据
  #     dm_ReadBy_ChartNo_GL_dealOne(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL = FParamL,page_size = 300)
  #     sql = paste0("select FchartNo,
  # FParamG,
  # FParamL,
  # FItemName,
  # FSubChartNo,
  # FkeyNo,
  # FLtab,
  # FItemModel,
  # FNote,
  # FIndexTxt,
  # FQty,
  # FLength,
  # FTotalQty
  # from  t_lcrds_bom
  # where FchartNo ='",FchartNo,"' and FParamG ='",FParamG,"'  and FParamL =  '",FParamL,"'
  #                  order by  FParamG,FParamL, FIndexTxt
  #                  ")
  #     res <- tsda::sql_select(conn,sql)
  #     if(nrow(res)>0){
  #       names(res) <-c('主图号','G番号-参数','L番号-参数','子项名称','分图号','子项件号','子项L番','子项规格',
  #                      '子项备注','子项序号','子项基本数量','子项长度/系数','子项总数量')
  #     }
  #   }
  #
  #
  #
  # }


  return(res)


  #针对数据符号SQL格式
#   lInput = FParamL
#   FParamL = sql_Ltab(FParamL)
#
#   if(FParamL == ''){
#     sql <- paste0("select
# FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,FParamG,
# FParamL = (
#         stuff(
#             (select ',' + FParamL from t_lcrds_bom where FchartNo = A.FchartNo
# 			and FIndexTxt = A.FIndexTxt  and FParamG =A.FParamG   for xml path('')),
#             1,
#             1,
#             ''
#         )
#     )
# from t_lcrds_bom as A
# where FchartNo ='",FchartNo,"'
# and A.FParamG ='",FParamG,"'
# group by FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,FParamG
# order by FIndexTxt")
#   }else{
#     sql <- paste0("select
# FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,FParamG,
# FParamL = (
#   stuff(
#     (select ',' + FParamL from t_lcrds_bom where FchartNo = A.FchartNo
#      and FIndexTxt = A.FIndexTxt  and FParamG =A.FParamG and FParamL in (",FParamL,")   for xml path('')),
#     1,
#     1,
#     ''
#   )
# )
# from t_lcrds_bom as A
# where FchartNo ='",FchartNo,"'
# and A.FParamG ='",FParamG,"'
# and A.FParamL in (",FParamL,")
# group by FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,FParamG
# order by FIndexTxt")
#   }



  # if(ncount >0){
  #   #针对有数据的情况
  #   res <- res[ ,c('FchartNo','FParamG','FParamL','FItemName','FSubChartNo','FkeyNo','FLtab','FItemModel',
  #                  'FNote','FIndexTxt','FQty','FLength','FTotalQty')]
  #   res$FParamL <- lInput
  #
  #   res2 <- split(res,res$FIndexTxt)
  #   #处理重复的情况
  #
  #   raw <-lapply(res2, function(data){
  #     ncount = nrow(data)
  #     if(ncount >1){
  #       find = 0
  #       for (i in 1:ncount) {
  #         value =  data[i,'FkeyNo']
  #         print(value)
  #
  #         type = bom_getVarValueType(value)
  #         if(type =='G' |type =='seq'){
  #           info = data[i,]
  #           find =1
  #           print(info)
  #         }
  #
  #       }
  #       if(find == 0){
  #         info = data[1,]
  #         print(info)
  #       }
  #
  #
  #     }else{
  #       info <- data
  #     }
  #     return(info)
  #   })
  #   res3 = do.call('rbind',raw)
  # }else{
  #   #针对没有数据的情况
  #   FchartNo2 = FchartNo
  #   FParamG2 = FParamG
  #   FParamL2 = FParamL
  #   FItemName = ""
  #   FSubChartNo =""
  #   FkeyNo  =""
  #   FLtab =""
  #   FItemModel =""
  #   FNote =""
  #   FIndexTxt =""
  #   FQty = 0
  #   FLength =0
  #   FTotalQty = 0
  #   res3 <- data.frame(FchartNo2,FParamG2,FParamL2,FItemName,FSubChartNo,FkeyNo,FLtab,
  #                       FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,stringsAsFactors = F)
  #
  # }




}




#' 针对图号进行查询，兼容DM清单结果
#'
#' @param conn 连接
#' @param FchartNo 图号
#' @param FParamG G番
#' @param FParamL L番
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_selectDB_detail_combo1()
dm_selectDB_detail_combo1 <- function(conn=tsda::conn_rds('lcrds'),FchartNo ='YX200A714', FParamG ='GS11'  , FParamL ='') {
  #针对数据符号SQL格式
  lInput = FParamL
  FParamL = sql_Ltab(FParamL)

  if(FParamL == ''){
    sql <- paste0("select
FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,FParamG,
FParamL = (
        stuff(
            (select ',' + FParamL from t_lcrds_bom where FchartNo = A.FchartNo
			and FIndexTxt = A.FIndexTxt  and FParamG =A.FParamG   for xml path('')),
            1,
            1,
            ''
        )
    )
from t_lcrds_bom as A
where FchartNo ='",FchartNo,"'
and A.FParamG ='",FParamG,"'
group by FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,FParamG
order by FIndexTxt")
  }else{
    sql <- paste0("select
FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,FParamG,
FParamL = (
  stuff(
    (select ',' + FParamL from t_lcrds_bom where FchartNo = A.FchartNo
     and FIndexTxt = A.FIndexTxt  and FParamG =A.FParamG and FParamL in (",FParamL,")   for xml path('')),
    1,
    1,
    ''
  )
)
from t_lcrds_bom as A
where FchartNo ='",FchartNo,"'
and A.FParamG ='",FParamG,"'
and A.FParamL in (",FParamL,")
group by FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,FParamG
order by FIndexTxt")
  }


  res <- tsda::sql_select(conn,sql)
  #针对数据进行处理
  ncount <- nrow(res)
  if(ncount >0){
    #针对有数据的情况
    res <- res[ ,c('FchartNo','FParamG','FParamL','FItemName','FSubChartNo','FkeyNo','FLtab','FItemModel',
                   'FNote','FIndexTxt','FQty','FLength','FTotalQty')]
    res$FParamL <- lInput

    res2 <- split(res,res$FIndexTxt)
    #处理重复的情况

    raw <-lapply(res2, function(data){
      ncount = nrow(data)
      if(ncount >1){
        find = 0
        for (i in 1:ncount) {
          value =  data[i,'FkeyNo']
          print(value)

          type = bom_getVarValueType(value)
          if(type =='G' |type =='seq'){
            info = data[i,]
            find =1
            print(info)
          }

        }
        if(find == 0){
          info = data[1,]
          print(info)
        }


      }else{
        info <- data
      }
      return(info)
    })
    res3 = do.call('rbind',raw)
  }else{
    #针对没有数据的情况
    FchartNo2 = FchartNo
    FParamG2 = FParamG
    FParamL2 = FParamL
    FItemName = ""
    FSubChartNo =""
    FkeyNo  =""
    FLtab =""
    FItemModel =""
    FNote =""
    FIndexTxt =""
    FQty = 0
    FLength =0
    FTotalQty = 0
    res3 <- data.frame(FchartNo2,FParamG2,FParamL2,FItemName,FSubChartNo,FkeyNo,FLtab,
                       FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,stringsAsFactors = F)

  }


  ncount_combo1 <- nrow(res3)
  FDmNo1 = rep('',ncount_combo1)
  FLevel1 = rep(0,ncount_combo1)
  FParentRow1 = rep("",ncount_combo1)
  FParentItemNo1 = rep('',ncount_combo1)
  FParentItemName1 = rep('',ncount_combo1)
  FParentQty1 = rep(0,ncount_combo1)
  FParentChartNo1 = rep('',ncount_combo1)
  FParentChartG1 = rep('',ncount_combo1)
  FParentChartL1 = rep('',ncount_combo1)

  data_combo1 <- data.frame(
    FDmNo1,FLevel1,FParentRow1,FParentItemNo1,FParentItemName1,FParentQty1,FParentChartNo1,FParentChartG1,FParentChartL1,stringsAsFactors = F


  )

  res4 <-cbind(data_combo1,res3)

  n1 <-c("DM单号","级数","上级行号","物料号","名称","用量数","图号","G番","L番")


  n2 <-c("主图号", "G番号-参数","L番号-参数","子项名称", "分图号","子项件号",
         "子项L番", "子项规格", "子项备注", "子项序号", "子项基本数量" , "子项长度/系数",
         "子项总数量")
  name_all <- c(n1,n2)

  names(res4) <- name_all
  return(res4)


}





