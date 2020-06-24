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
  sql <- paste0("select FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FQty,FGtab  from t_lcrds_gtab where FchartNo ='",FchartNo,"'")
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
    #针对数据处理处理,其中数量的na给予0处理
    r$FQty <- tsdo::na_replace(r$FQty,0)
    r$FLength <- tsdo::na_replace(r$FLength,0)

    r$FTotalQty <- r$FQty * r$FLength
    r$FParamG <- r$FGtab
    r$FParamL <- FLtab
    #针对空行进行处理,删除空行
    r <- r[!is.na(r$FQty),]
    #针对列进行处理
    Gtab_colnames <- names(r)
    Gtab_colNames_sel <- !Gtab_colnames %in% 'FGtab'
    r <- r[ ,Gtab_colNames_sel]

  }else{
    r<-NA
  }

  return(r)



}


#' 按图号读取数据
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_ReadBy_ChartNo()
dm_ReadBy_ChartNo <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672'){
  FLtabs <-Ltab_get_uniqueMembers(conn,FchartNo)
  raw <- lapply(FLtabs, function(FLtab){
    res <- dm_ReadBy_ChartNo_Ltab(conn=conn,FchartNo = FchartNo,FLtab = FLtab)
    return(res)
  })
  data <- do.call('rbind',raw)
  return(data)


}

#' 将图号数据写入数据库
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_writeDB_ChartNo()
dm_writeDB_ChartNo <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SYE601B672'){
   data <- dm_ReadBy_ChartNo(conn=conn,FchartNo = FchartNo)
   #删除要备份的数据
   try({
     dm_chartNo_deleteDB(conn=conn,FchartNo = FchartNo)
   })

   #插入新的数据
   #tsda::upload_data(conn,'t_lcrds_bom',data)
   #不做判断,直接写入数据
   #这个地方可能是性能出现问题的重点
   tsda::db_writeTable(conn=conn,table_name = 't_lcrds_bom',r_object = data,append = T)



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



#' 写入数据库
#'
#' @param conn 连接
#' @param show_process 是否显示进度
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_dealAll2()
dm_dealAll2 <- function(conn=tsda::conn_rds('lcrds'),show_process=FALSE) {
  chartNos <- dm_getToDoChartNo(conn)
  ncount <- length(chartNos)
  if(!is.na(chartNos)){
    #在存在数据的情况下处理
    #start

    if(show_process){

      withProgress(message = 'BOM运算处理中', value = 0, {
        for (i in 1:ncount) {
          FchartNo <-chartNos[i]
          #写入数据
          dm_writeDB_ChartNo(conn=conn,FchartNo = FchartNo)

          #更新进度条
          incProgress(1/ncount, detail = paste("(",i,"/",ncount,")..."))
          #更新记录
          db_bom_setUpdate(conn,FchartNo)



        }

      })




    }else{

      for (FchartNo in chartNos) {

        #写入数据
        dm_writeDB_ChartNo(conn=conn,FchartNo = FchartNo)

        #更新记录
        db_bom_setUpdate(conn,FchartNo)


      }

    }




    #end
  }



}


#' 查询信息
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
#' dm_selectDB_detail()
dm_selectDB_detail <- function(conn=tsda::conn_rds('lcrds'),FchartNo ='P235009B198', FParamG ='G11'  , FParamL ='L42') {

  if(is.na(FParamL)){
    sql <- paste0("select
FchartNo 主图号
,FParamG [G番号-参数]
,FParamL [L番号-参数]
,FItemName 子项名称
,FSubChartNo 分图号
,FkeyNo 子项件号
,FLtab 子项L番
,FItemModel 子项规格
,FNote 子项备注
,FIndexTxt 子项序号
,FQty 子项基本数量
,FLength [子项长度/系数]
,FTotalQty 子项总数量
from t_lcrds_bom
where FchartNo ='",FchartNo,"' and FParamG ='",FParamG,"'  order by FIndexTxt")
  }else{
    sql <- paste0("select
FchartNo 主图号
,FParamG [G番号-参数]
,FParamL [L番号-参数]
,FItemName 子项名称
,FSubChartNo 分图号
,FkeyNo 子项件号
,FLtab 子项L番
,FItemModel 子项规格
,FNote 子项备注
,FIndexTxt 子项序号
,FQty 子项基本数量
,FLength [子项长度/系数]
,FTotalQty 子项总数量
from t_lcrds_bom
where FchartNo ='",FchartNo,"' and FParamG ='",FParamG,"'  and FParamL ='",FParamL,"'order by FIndexTxt")
  }


  res <- tsda::sql_select(conn,sql)
  return(res)

}





#' 读取DM清单
#'
#' @param file 文件
#' @param sheet 页答名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_read_list()
dm_read_list <- function(file="data-raw/bom_src4.xlsx",sheet = "DM清单"){

  data <- readxl::read_excel(file,
                         sheet = sheet)
  col_names_selected <-c("DM单号","级数","上级行号","物料号","名称","用量数","图号","G番","L番")
  data <- data[,col_names_selected]
  class(data) <-'data.frame'
  return(data)
}


#' 查询所有清单
#'
#' @param file 文件
#' @param sheet 页答
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dm_queryAll()
dm_queryAll <-function(file="data-raw/bom_src4.xlsx",sheet = "DM清单",conn=tsda::conn_rds('lcrds')){
  data <- dm_read_list(file=file,sheet = sheet)
  ncount <- nrow(data)
  if(ncount >0){
    res <- lapply(1:ncount, function(i){
      item <- data[i,]
      FchartNo =  data[i,"图号"]
      FParamG = data[i,"G番"]
      FParamL = data[i,"L番"]
      r <- dm_selectDB_detail(conn = conn,FchartNo = FchartNo,FParamG = FParamG,FParamL =FParamL)
      ncount_item <- nrow(r)
      if(ncount_item >0){
        item_1 <-tsdo::df_rowRepMulti(item,ncount_item)
        item_res <- cbind(item_1,r)
      }else{

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
        item2 <- data.frame(FchartNo2,FParamG2,FParamL2,FItemName,FSubChartNo,FkeyNo,FLtab,
                            FItemModel,FNote,FIndexTxt,FQty,FLength,FTotalQty,stringsAsFactors = F)
        names(item2) <-c("主图号", "G番号-参数","L番号-参数","子项名称", "分图号","子项件号",
                          "子项L番", "子项规格", "子项备注", "子项序号", "子项基本数量" , "子项长度/系数",
                          "子项总数量")

        item_res <- cbind(item,item2)


      }
      return(item_res)
    })
    data2 <- do.call('rbind',res)

    return(data2)


  }

}






