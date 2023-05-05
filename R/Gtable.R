#' 读取G番表
#'
#' @param file 文件
#' @param sheetName 页答名称
#' @param max_row 最大行数，默认99
#'
#' @return 返回值
#' @export
#'
#' @examples
#' lc_read_G_table()
Gtab_read <- function(file="./data-raw/bom_src.xlsx",sheetName="P235911B000",max_row=198,rm.na=TRUE) {
  bom_src <- read_excel(file,
                        sheet = sheetName, n_max = max_row)
  #针对数据行进行判断,防止出错
  ncount <- nrow(bom_src)
  if(ncount >0){
    #have data
    index <- bom_src$主图号
    data <- bom_src[!is.na(index),]
    class(data) <-"data.frame"
    allColNames <- names(data)
    fixedColNames=Gtab_fixedColNames()
    varColNames = allColNames[!allColNames %in% fixedColNames]
    col_count <- length(allColNames)
    col_fixed_count <- length(fixedColNames)
    col_var_count <- length(varColNames)
    row_count <- nrow(data)


    #names(bb)[! names(bb)  %in%  Gtab_fixedColNames()]
    #处理na问题
    col_not_na <-lapply(varColNames, function(var_colName){
      item <- data[,var_colName]
      res <-!tsdo::is_all_true(is.na(item))
      return(res)
    })
    varColNames_not_na <- varColNames[unlist(col_not_na)]
    allColNames_not_na <- c(fixedColNames,varColNames_not_na)
    if(rm.na){
      data <- data[,allColNames_not_na]
      allColNames <- allColNames_not_na
      varColNames <- varColNames_not_na

      col_count <- length(allColNames)
      col_var_count <- length(varColNames)
    }
    #进行标准化处理
    data_melt <- melt(data=data,id.vars = fixedColNames,measure.vars = varColNames,variable.name = 'G番',value.name = '数量',
                      na.rm = T,

                      factorsAsStrings=FALSE)
    #针对数据类型进一步处理
    data_melt[,'G番'] <-as.character(data_melt[,'G番'])

    data_melt[,'件号'] <-as.character(data_melt[,'件号'])
    data_melt[,'件号'] <-tsdo::na_replace(data_melt[,'件号'],"")
    #增加对分图号的处理null
    data_melt[,'分图号'] <-tsdo::na_replace(data_melt[,'分图号'],"")
    data_melt[,'件号'] <-tsdo::na_replace(data_melt[,'件号'],"")
    data_melt[,'L番'] <-as.character(data_melt[,'L番'])
    data_melt[,'L番'] <-tsdo::na_replace(data_melt[,'L番'],"")
    data_melt[,'备注'] <-as.character(data_melt[,'备注'])
    data_melt[,'备注'] <-tsdo::na_replace(data_melt[,'备注'],"")
    data_melt[,'规格'] <-as.character(data_melt[,'规格'])
    data_melt[,'规格'] <-tsdo::na_replace(data_melt[,'规格'],"")
    data_melt[,'数量'] <-as.character(data_melt[,'数量'])

    res <- list(data_raw=data,
                data_norm=data_melt,

                fixedColNames=fixedColNames,
                varColNames=varColNames,
                allColNames=allColNames,
                col_count=col_count,
                col_fixed_count=col_fixed_count,
                col_var_count=col_var_count,
                row_count=row_count)
  }else{
    #no data
    res <- list(data_raw=NULL,
                data_norm=NULL,

                fixedColNames='',
                varColNames='',
                allColNames='',
                col_count=0,
                col_fixed_count=0,
                col_var_count=0,
                row_count=0)
  }


  return(res)
}

#' G表的表头
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_fixedColNames()
Gtab_fixedColNames <- function(){
  res <-c("主图号", "名称",   "分图号", "件号",   "L番",    "规格",   "备注" ,  "序号")
  return(res)
}


#' G表列标题名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_normColNames()
Gtab_normColNames <- function(){
  res <-c("主图号", "名称",   "分图号", "件号",   "L番",    "规格",   "备注" ,  "序号","G番",'数量')
  return(res)
}

#' G表数据库名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_normColNames_db()
Gtab_normColNames_db <- function(){
  res <-c("FchartNo", "FItemName","FSubChartNo","FkeyNo","FLtab","FItemModel","FNote",'FIndexTxt',"FGtab","FQty")
  return(res)
}


#' G表写入
#'
#' @param conn 连接
#' @param file  文件
#' @param sheetName  页签
#' @param max_row 最大行
#' @param rm.na 删除空格
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_write_db()
Gtab_write_db <- function(conn=tsda::conn_rds('lcrds'),file="./data-raw/bom_src.xlsx",sheetName="P235911B000",max_row=198,rm.na=TRUE,is_force) {

  r <- Gtab_read(file = file,sheetName = sheetName,max_row = max_row,rm.na = rm.na)
  data <- r$data_norm
  ncount <- r$row_count
  if(ncount>0){
    #处理并上传数据库服务器
    names(data) <- Gtab_normColNames_db()
    #删除已有的数据，然后再上传
    FchartNo <- unique(data$FchartNo)
    Gtab_delete_db(conn,FchartNo)
    #再上传数据
    tsda::upload_data(conn,'t_lcrds_gtab',data = data)
    #写入上传日志
    sql_update <- paste0("insert into t_lcrds_UploadLog (FchartNo) values('",FchartNo,"')")
    tsda::sql_update(conn,sql_update)
  }

}


#' G表批量写入数据库
#'
#' @param conn 连接
#' @param file 文件
#' @param show_progress 是否显示进度
#' @param max_row 最大行
#' @param rm.na 删除空行
#' @param include_sheetNames 只更新指定页签
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_batchWrite_db()
Gtab_batchWrite_db <- function(conn=tsda::conn_rds('lcrds'),file="./data-raw/bom_src.xlsx",include_sheetNames=NA,show_progress=FALSE,max_row=198,rm.na=TRUE){
  sheetNames_all <- tsda::excel_getSheetNames(file)
  exclude_sheetName <-lc_exclude_sheetNames()
  #排除不需要的页签
  if(is.na(include_sheetNames)){
    #全部页签
    sheetNames_selected <- sheetNames_all[!sheetNames_all %in% exclude_sheetName]
  }else{
    #指定页签
    str_split <-strsplit(include_sheetNames,",")
    sheetNames_selected <- str_split[[1]]
    }


  ncount <- length(sheetNames_selected)
  if(show_progress){
    #显示进度条
    withProgress(message = 'G番表批量处理中', value = 0, {

      lapply(1:ncount, function(i){
        sheetName <-sheetNames_selected[i]

        try({
          Gtab_write_db(conn =conn  ,file =file ,sheetName = sheetName  ,max_row = max_row,rm.na =rm.na )
          incProgress(1/ncount, detail = paste("(",i,"/",ncount,")..."))
        })
        print(sheetName)



      })

    })




  }else{
    #不显示
    lapply(1:ncount, function(i){
      sheetName <-sheetNames_selected[i]
      try({
        Gtab_write_db(conn =conn  ,file =file ,sheetName = sheetName  ,max_row = max_row,rm.na =rm.na )

      })
      print(sheetName)
    })
  }




}


#' 从数据库中读取G表
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FGtab G番号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_select_db()
Gtab_select_db <- function(conn=tsda::conn_rds('lcrds'),FchartNo='P235067C156',FGtab='G01') {
  sql <- paste0("SELECT FchartNo
      ,FItemName
      ,FSubChartNo
      ,FkeyNo
      ,FLtab
      ,FItemModel
      ,FNote
      ,FIndexTxt
      ,FGtab
      ,FQty
  FROM t_lcrds_gtab where FchartNo ='",FchartNo,"'  and FGtab ='",FGtab,"'")
  res <- tsda::sql_select(conn,sql)
  return(res)

}

#' 从数据库中读取G表,用于计算BOM
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FGtab G番号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_select_db_calc()
Gtab_select_db_calc <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SE304A200',FGtab='G01') {
  sql <- paste0("select
FchartNo,
isnull(FItemName,'') as  FItemName,
isnull(FSubChartNo,'') as    FSubChartNo ,
isnull(FkeyNo,'') as    FkeyNo  ,
isnull(FLtab,'') as    FLtab,
isnull(FItemModel,'') as  FItemModel,
isnull(FNote,'') as    FNote,
FIndexTxt,
FQty,
FGtab  from t_lcrds_gtab where FchartNo ='",FchartNo,"'  and FGtab ='",FGtab,"'")
  res <- tsda::sql_select(conn,sql)
  return(res)

}


#' 从数据库中读取G表,用于计算G表的统计信息
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FGtab G番号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_select_db_stat()
Gtab_select_db_stat <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SE304A200',FGtab='G01') {
  #计取G表
  data_G = Gtab_select_db_calc(conn = conn,FchartNo = FchartNo,FGtab = FGtab)
  #计取L表所有变量
  vars = Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
  #计算变量

  data_G$FkeyNo_tag = variable_search(data_G$FkeyNo,vars)
  data_G$FLtab_tag = variable_search(data_G$FLtab,vars)
  data_G$FQty_tag     = variable_search(data_G$FQty,vars)
  data_G$FTagCount_dim = data_G$FkeyNo_tag + data_G$FLtab_tag
  data_G$FTagCount_value = data_G$FQty_tag
  data_G$FTagCount_total = data_G$FTagCount_dim + data_G$FTagCount_value
  return(data_G)
}

#' 从数据库中读取G表,用于计算G表的统计信息,写入数据库
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FGtab G番号
#' @param page_size 分批写入数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_select_db_stat()
Gtab_write_db_stat <- function(conn=tsda::conn_rds('lcrds'),FchartNo='SE304A200',FGtab='G01',page_size = 200) {
  #计取数据
  data  = Gtab_select_db_stat(conn = conn,FchartNo = FchartNo,FGtab = FGtab)
  ncount = nrow(data)
  if(ncount >0){
    #当存在数据的情况下

    #step1 备份原来的数据,不存在性能问题
    sql_bak <- paste0("
insert into t_lcrds_gtabStat_Del
select * from t_lcrds_gtabStat
where FchartNo ='",FchartNo,"' and FGtab ='",FGtab,"'")
    tsda::sql_update(conn,sql_bak)

    #删除原有的数据，删除数据也可以接受
    sql_del <- paste0("
delete  from t_lcrds_gtabStat
where FchartNo ='",FchartNo,"' and FGtab ='",FGtab,"'")
    tsda::sql_update(conn,sql_del)
   #插入新的数据,可能有性能问题
    pageInfo =  tsdo::paging_setting(volume = ncount,each_page = page_size)
    pageCount = nrow(pageInfo)
    lapply(1:pageCount, function(i){
      from = pageInfo$FStart[i]
      to = pageInfo$FEnd[i]
      item =  data[from:to, ]
      #写入数据
      try({
        tsda::db_writeTable(conn = conn,table_name = 't_lcrds_gtabStat',r_object = item,append = T)
      })

    })



  }

return(data)
}









#' 从数据库中读取G表
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_select_db()
Gtab_selectDB_byChartNo <- function(conn=tsda::conn_rds('lcrds'),FchartNo='P235067C156') {
  sql <- paste0("SELECT FchartNo
      ,FItemName
      ,FSubChartNo
      ,FkeyNo
      ,FLtab
      ,FItemModel
      ,FNote
      ,FIndexTxt
      ,FGtab
      ,FQty
  FROM t_lcrds_gtab where FchartNo ='",FchartNo,"'")
  res <- tsda::sql_select(conn,sql)
  return(res)

}



#' G表删除
#'
#' @param conn 逻辑
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_delete_db()
Gtab_delete_db <- function(conn=tsda::conn_rds('lcrds'),FchartNo='P235067C156'){
  #back up del
  sql_bak <- paste0("insert into t_lcrds_gtabDel (FchartNo,FItemName,FSubChartNo,FkeyNo,FLtab,FItemModel,FNote,FIndexTxt,FGtab,FQty)
select *from t_lcrds_gtab where FchartNo = '",FchartNo,"'")
  tsda::sql_update(conn,sql_bak)
  #exec del
  sql_del <- paste0("delete from t_lcrds_gtab where FchartNo = '",FchartNo,"'")
  tsda::sql_update(conn,sql_del)
  res <- TRUE
  return(res)
}



#' 判断G表图号是否为新的内容
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_is_new()
Gtab_is_new <- function(conn=tsda::conn_rds('lcrds') ,FchartNo="P235067C156") {
  sql <- paste0("select FchartNo   from t_lcrds_gtab where FchartNo = '",FchartNo,"'")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if (ncount >0){
    info <- FALSE
  }else{
    info <- TRUE
  }
  return(info)

}


#' 获取G表中的唯一元素
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_get_uniqueMembers()
Gtab_get_uniqueMembers <- function(conn=tsda::conn_rds('lcrds'),FchartNo='YE603A049'){
  sql <- paste0("select distinct  FGtab  from t_lcrds_gtab where FchartNo ='",FchartNo,"'")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    info <- res$FGtab
  }else{
    info <- NA
  }
  return(info)
}



#' 查询相关
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Gtab_selectDB_byChartNo2()
Gtab_selectDB_byChartNo2 <- function(conn=tsda::conn_rds('lcrds'),FchartNo='YE603A049') {
  sql <- paste0("select

FchartNo 主图号
,FItemName 名称
,FSubChartNo 分图号
,FkeyNo 件号
,FLtab L番
,FItemModel 规格
,FNote 备注
,FIndexTxt 序号
,FGtab G番
,FQty 数量


from  t_lcrds_gtab where FchartNo='",FchartNo,"'
order by FchartNo,FGtab,FIndexTxt")
  res <- tsda::sql_select(conn,sql)
  return(res)

}




