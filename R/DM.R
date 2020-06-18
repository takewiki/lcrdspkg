#deal with dm

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

#' 针对上传的图号完成处理后设置标志为1
#'
#' @param conn 连接
#' @param FchartNo  主图号
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' db_bom_setUpdate()
db_bom_setUpdate <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672'){
  sql <- paste0("update  a  set a.FIsDo = 1  from t_lcrds_uploadLOG  a  where FIsDo =0 and FchartNo ='",FchartNo,"'")
  tsda::sql_update(conn,sql)

}




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
  sql <- paste0("select FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty  from t_lcrds_gtab where FchartNo ='",FchartNo,"' and FGtab ='",FGtab,"'")
  r <- tsda::sql_select(conn,sql)
  #print(r)
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
    r$FParamG <- FGtab
    r$FParamL <- FLtab
    #针对空行进行处理,删除空行
    r <- r[!is.na(r$FQty),]
    print(r)
    #写入数据库
    try({
      dm_bom_deleteDB(conn = conn,FchartNo = FchartNo,FGtab = FGtab,FLtab = FLtab)
    })

    #写入数据库
    print('DB')
    tsda::upload_data(conn,'t_lcrds_bom',r)


  }else{
    r<-NA
  }

  return(r)



}

#' 获取待处理的图号
#'
#' @param conn 连接
#'
#' @return 返回主图事情
#' @export
#'
#' @examples
#' dm_getToDoChartNo()
dm_getToDoChartNo <- function(conn=tsda::conn_rds('lcrds')) {
  sql <- paste0("select FchartNo from t_lcrds_UploadLog where FIsDo = 0")
  data <- tsda::sql_select(conn,sql)
  ncount <-nrow(data)
  if(ncount>0){
    res <- data$FchartNo
  }else{
    res <-NA
  }
  return(res)
}


#' 更新所有BOM
#'
#' @param conn 连接
#' @param show_process 是否显示进度
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_dealAll()
dm_dealAll <- function(conn=tsda::conn_rds('lcrds'),show_process=FALSE) {

  chartNos <- dm_getToDoChartNo(conn)
  ncount <- length(chartNos)
  if(!is.na(chartNos)){
    #在存在数据的情况下处理
    #start

    if(show_process){

      withProgress(message = 'BOM运算处理中', value = 0, {
        for (i in 1:ncount) {
          FchartNo <-chartNos[i]

          FGtabs <-Gtab_get_uniqueMembers(conn,FchartNo)

          g_logical <- ! is.na(FGtabs[1])

          FLtabs <-Ltab_get_uniqueMembers(conn,FchartNo)

          l_logical <- ! is.na(FLtabs[1])
          #全部不为空时处理

          if( g_logical & l_logical){

            for (FGtab in FGtabs) {
              for (FLtab in FLtabs) {

                try(  dm_dealOne(conn,FchartNo,FGtab,FLtab))



              }

            }

          }



          #更新进度条
          incProgress(1/ncount, detail = paste("(",i,"/",ncount,")..."))
          #更新记录
          db_bom_setUpdate(conn,FchartNo)



        }

      })




    }else{

      for (FchartNo in chartNos) {

        FGtabs <-Gtab_get_uniqueMembers(conn,FchartNo)

        g_logical <- ! is.na(FGtabs[1])

        FLtabs <-Ltab_get_uniqueMembers(conn,FchartNo)

        l_logical <- ! is.na(FLtabs[1])
        #全部不为空时处理

        if( g_logical & l_logical){

          for (FGtab in FGtabs) {
            for (FLtab in FLtabs) {

              try(  dm_dealOne(conn,FchartNo,FGtab,FLtab))



            }

          }

        }





      #更新记录
        db_bom_setUpdate(conn,FchartNo)


      }

    }




    #end
  }



}
