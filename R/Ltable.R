#' 读取L番表
#'
#' @param file 文件名称xlsx
#' @param sheetName 页答名称
#' @param skip_rows 一般从第200行开始，跳过199行
#' @param rm.na是否去除
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_read()
Ltab_read <- function(file="./data-raw/bom_src.xlsx",sheetName="YE604B797",skip_rows=199,rm.na=TRUE) {
  bom_src <- read_excel(file,
                        sheet = sheetName, skip = skip_rows)
  #针对数据行进行判断,防止出错
  ncount <- nrow(bom_src)
  if(ncount >0){
    #have data
    index <- bom_src$主图号
    data <- bom_src[!is.na(index),]
    class(data) <-"data.frame"
    allColNames <- names(data)
    fixedColNames=Ltab_fixedColNames()
    varColNames = allColNames[!allColNames %in% fixedColNames]
    col_count <- length(allColNames)
    col_fixed_count <- length(fixedColNames)
    col_var_count <- length(varColNames)
    row_count <- nrow(data)
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
    data_melt <- melt(data=data,id.vars = fixedColNames,measure.vars = varColNames,variable.name = '件号',value.name = '长度',
                      na.rm = T,

                      factorsAsStrings=FALSE)
    #针对数据类型进行处理
    data_melt[,'件号'] <-as.character(data_melt[,'件号'])
    data_melt[,'长度'] <-as.character(data_melt[,'长度'])
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


#' 读取L番表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_fixedColNames()
Ltab_fixedColNames <- function(){
  res <-c("主图号", "L番")
  return(res)
}

#' L图中文版
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_normColNames()
Ltab_normColNames <- function(){
  res <-c("主图号", "L番","件号","长度")
  return(res)
}


#' L表英文版
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_normColName_db()
Ltab_normColNames_db <- function(){
  res <-c("FchartNo", "FLtab","FkeyNo","FLength")
  return(res)
}

#' 写入数据库
#'
#' @param conn  连接
#' @param file 文件
#' @param sheetName 页签名
#' @param skip_rows 行数
#' @param rm.na 去除空
#'
#' @return 返回值
#' @import readxl
#' @import reshape2
#' @export
#'
#' @examples
#' Ltab_write_db()
Ltab_write_db <- function(conn=tsda::conn_rds('lcrds'),file="./data-raw/bom_src.xlsx",sheetName="YE604B797",skip_rows=199,rm.na=TRUE) {

  r <- Ltab_read(file=file,sheetName = sheetName,skip_rows = skip_rows,rm.na = rm.na)
  data <- r$data_norm
  ncount <- r$row_count
  if(ncount>0){
    #处理并上传数据库服务器
    names(data) <- Ltab_normColNames_db()
    #删除已有的数据，然后再上传
    FchartNo <- unique(data$FchartNo)
    Ltab_delete_db(conn,FchartNo)
    #再上传数据
    tsda::upload_data(conn,'t_lcrds_ltab',data = data)
  }

}

#' 批量写入L番表进入数据库
#'
#' @param conn 连接
#' @param file 文件
#' @param exclude_sheetName 排除页签
#' @param show_progress 是否显示进度
#' @param skip_rows 跳过行数
#' @param rm.na 删除空行
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_batchWrite_db()
Ltab_batchWrite_db <- function(conn=tsda::conn_rds('lcrds'),file="./data-raw/bom_src.xlsx",include_sheetNames=NA,show_progress=FALSE,skip_rows=199,rm.na=TRUE){
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
    withProgress(message = 'L番表批量处理中', value = 0, {

      lapply(1:ncount, function(i){
        sheetName <-sheetNames_selected[i]

        try({
          Ltab_write_db(conn =conn ,file = file,sheetName = sheetName,skip_rows =skip_rows ,rm.na =rm.na )
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
        Ltab_write_db(conn =conn ,file = file,sheetName = sheetName,skip_rows =skip_rows ,rm.na =rm.na )

      })
      print(sheetName)
      })
  }




}

#' L翻表
#'
#' @param conn 连接
#' @param FchartNo  主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_select_db()
Ltab_select_db <- function(conn=tsda::conn_rds('lcrds'),FchartNo='YE603A049') {
  sql <- paste0("select FchartNo,FLtab,FkeyNo,FLength from t_lcrds_ltab
where FchartNo = '",FchartNo,"'")
  data <- tsda::sql_select(conn,sql)
  return(data)
}


#' 查询L表相关信息
#'
#' @param conn  连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_select_db2()
Ltab_select_db2 <- function(conn=tsda::conn_rds('lcrds'),FchartNo='YE603A049') {
  sql <- paste0("select FchartNo,FLtab,FkeyNo,FLength from t_lcrds_ltab
where FchartNo = '",FchartNo,"'")
  data <- tsda::sql_select(conn,sql)
  names(data) <- Ltab_normColNames()
  return(data)
}




#' 检查L番是否存在
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_checkExist()
Ltab_checkExist <- function(conn=tsda::conn_rds('lcrds'),FchartNo='YE603A049') {
  sql <- paste0("  select count(1) as FCount from t_lcrds_ltab
where FchartNo = '",FchartNo,"'")
  data <- tsda::sql_select(conn,sql)
  ncount = data$FCount
  if(ncount >0){
    res = TRUE
  }else{
    res = FALSE
  }
  return(res)
}



#' 检查具体的某个L番是否存在
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FLtab  具体的L番表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_checkExist_one()
Ltab_checkExist_one <- function(conn=tsda::conn_rds('lcrds'),FchartNo='P203031B120',FLtab ='L04') {
  sql <- paste0("  select count(1) as FCount from t_lcrds_ltab
where FchartNo = '",FchartNo,"'  and FLtab ='",FLtab,"'")
  data <- tsda::sql_select(conn,sql)
  ncount = data$FCount
  if(ncount >0){
    res = TRUE
  }else{
    res = FALSE
  }
  return(res)
}



#' L表获取唯一变量
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return 返回唯一变量
#' @export
#'
#' @examples
#' Ltab_get_uniqueVars()
Ltab_get_uniqueVars <- function(conn=tsda::conn_rds('lcrds'),FchartNo='YE603A049'){
  sql <- paste0("select distinct FkeyNo from t_lcrds_ltab where FchartNo = '",FchartNo,"'")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    info <- res$FkeyNo
  }else{
    info <- NA
  }
  return(info)
}


#' 获取L表不重复元素
#'
#' @param conn 连接
#' @param FchartNo 主图号
#'
#' @return
#' @export
#'
#' @examples
#' Ltab_get_uniqueMembers()
Ltab_get_uniqueMembers <- function(conn=tsda::conn_rds('lcrds'),FchartNo='YE603A049'){
  sql <- paste0("select distinct FLtab   from t_lcrds_ltab where FchartNo = '",FchartNo,"'")
  res <- tsda::sql_select(conn,sql)
  ncount <- nrow(res)
  if(ncount >0){
    info <- res$FLtab
  }else{
    #没有查询到,返回了NA信息
    #info <- NA
    #调整后将不存在L番设置为''
    info <- ''
  }
  return(info)
}



#' 获取L表的变量值
#'
#' @param conn 连接
#' @param FchartNo 主图号
#' @param FLtab L番值
#' @param FkeyNo 变量值
#'
#' @return 返回具有数值
#' @export
#'
#' @examples
#' Ltab_get_varValue()
Ltab_get_varValue <- function(conn=tsda::conn_rds('lcrds'),FchartNo = 'SYE601B672',FLtab ='L02',FkeyNo ='A1L'){
  sql <- paste0("select FLength  from t_lcrds_ltab where FchartNo = '",FchartNo,"' and FLtab ='",FLtab,"'  and FkeyNo ='",FkeyNo,"'")
  r <- tsda::sql_select(conn,sql)
  ncount <- nrow(r)
  if(ncount >0){
    res <- r$FLength
  }else{
    res <- NA
  }
  return(res)
}


#' 获取L表的变量值批量
#'
#' @param conn 连接
#' @param FLtabBatch 可能多L番
#' @param FchartNo 主图号
#'
#' @return 返回具有数值
#' @export
#'
#' @examples
#' Ltab_get_varValueBatch()
Ltab_get_varValueBatch <- function(conn=tsda::conn_rds('lcrds'),FchartNo = 'SE304A200',FLtabBatch ='L33,L67'){
  #针对L番进行处理
  #
  FLtab_format_sql = tsdo::sql_str2(strsplit(FLtabBatch,',')[[1]])
  sql <- paste0("select  FLtab,FkeyNo,FLength  from t_lcrds_ltab where FchartNo = '",FchartNo,"' and FLtab in (",FLtab_format_sql,")
                order by   FkeyNo,FLtab
                ")
  print(sql)
  res <- tsda::sql_select(conn,sql)

  return(res)
}


#' L表删除
#'
#' @param conn 逻辑
#' @param FchartNo 主图号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Ltab_delete_db()
Ltab_delete_db <- function(conn=tsda::conn_rds('lcrds'),FchartNo='P235067C156'){
  #back up del
  sql_bak <- paste0("insert into t_lcrds_ltabDel(FchartNo,FLtab,FkeyNo,FLength)

select FchartNo,FLtab,FkeyNo,FLength   from t_lcrds_ltab
where FchartNo='",FchartNo,"'")
  tsda::sql_update(conn,sql_bak)
  #exec del
  sql_del <- paste0("delete from t_lcrds_ltab where FchartNo = '",FchartNo,"'")
  tsda::sql_update(conn,sql_del)
  res <- TRUE
  return(res)
}


