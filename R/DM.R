#deal with dm


#' 处理单个dm清单
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FGtab G表
#' @param FLtab L表值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_dealOne()
dm_dealOne <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672',FGtab ='G10',FLtab ='L02') {

  #读取基础表
  sql <- paste0("select FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty  from t_lcrds_gtab
where FchartNo ='",FchartNo,"' and FGtab ='",FGtab,"'")
  r <- tsda::sql_select(conn,sql)
  print(r)
  ncount <- nrow(r)
  if(ncount>0){
    #针对整体数据进行处理
    r$FLength <- 1
    for (i in 1:ncount) {
      #针对每一行数据进行处理
      keyNo <- tsdo::na_replace(r[i,'FkeyNo'],'')
      #针对件号进行处理
      if(tsdo::len(keyNo) >0){
        #获取所有的变量变量
        vars <-Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
        if( keyNo %in% vars){
          #需要处理
          length_value <- Ltab_get_varValue(conn = conn,FchartNo = FchartNo,FLtab = FLtab,FkeyNo = keyNo)
          length_value <- as.numeric(length_value)
          #针对值进行处理
          r[i,'FLength'] <- length_value
        }

      }
      #针对L翻进行处理
      ltab <-tsdo::na_replace(r[i,'FLtab'],'')
      if(tsdo::len(ltab)){
        vars <-Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
        if( ltab %in% vars){
          #针对数据处理处理
          length_value <- Ltab_get_varValue(conn = conn,FchartNo = FchartNo,FLtab = FLtab,FkeyNo = ltab)
          print(length_value)
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
    r$FTotalQty <- r$FQty * r$FLength
    #针对空行进行处理,删除空行
    r <- r[!is.na(r$FQty),]

  }else{
    r<-NA
  }

  return(r)



}
