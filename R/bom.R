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
dm_ReadBy_ChartNo_Ltab <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672',FLtab ='L02') {

  #读取基础表,减少对G的依赖
  #所有的G表一起处理
  sql <- paste0("select FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FGtab  from t_lcrds_gtab where FchartNo ='",FchartNo,"'")
  #读取数据,其中r表示G表
  r <- tsda::sql_select(conn,sql)
  #print(r)
  ncount <- nrow(r)
  if(ncount>0){
    #针对整体数据进行处理
    r$FLength <- 1
    for (i in 1:ncount) {
      #针对每一行数据进行处理,
      #针对件号进行处理
      keyNo <- tsdo::na_replace(r[i,'FkeyNo'],'')
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
                  length_value <- as.numeric(length_value)
                  #针对值进行处理
                  r[i,'FLength'] <- length_value

          }else{
            #设置相应的长度为0
            r[i,'FLength'] <- 0
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




    }

    #数据已经处理完了
    r$FQty <- as.numeric(r$FQty)
    r$FLength <- as.numeric(r$FLength)
    #针对数据处理处理,其中数量的na给予0处理
    r$FQty <- tsdo::na_replace(r$FQty,0)
    r$FLength <- tsdo::na_replace(r$FLength,0)

    r$FTotalQty <- r$FQty * r$FLength
    r$FParamG <- r$FGtab
    r$FParamL <- FLtab
    #针对空行进行处理,删除空行
    #针对汇总行也进行相应的处理

    # r <- r[!is.na(r$FQty),]
    # r <- r[!is.na(r$FTotalQty),]
     r <- r[r$FQty  > 0 , ]
     r <- r[r$FTotalQty > 0 , ]
    #针对列进行处理
    Gtab_colnames <- names(r)
    Gtab_colNames_sel <- !Gtab_colnames %in% 'FGtab'
    r <- r[ ,Gtab_colNames_sel]

  }else{
    r<-NA
  }

  return(r)



}
