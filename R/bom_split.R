#' 针对BOM数据进行拆分处理
#'
#' @param mtrl_multiple_G 待处理的物料包含多个G
#' @param maxId 最大的内码号
#'
#' @return 返回一个处理后的数据框
#' @export
#'
#' @examples
#' bom_split()
bom_split <- function(mtrl_multiple_G= c('P203031A112G08G02G01G21G34G10G06G44G31','P203031A112G08G02G01G21G34G10G06G44G32'),
                      maxId=0) {

  aa <- mtrl_multiple_G
  #获取物料长度
  ncount_aa <- length(aa)
  #按字母G拆开,结果为list
  bb <-strsplit(aa,'G')
  #针对结果添加名称
  names(bb) <- aa
  #针对数据进行分行处理
  mydata <- lapply(1:ncount_aa, function(i){
    #针对每一行数据进行处理
    data <- bb[[i]]
    data_head <- aa[i]
    ncount <- length(data)
    if(ncount >1){
      data_share <- data[1]
      data_entry <- data[2:ncount]
      ncount_entry <- length(data_entry)
      data_body <-paste0(data_share,'G',data_entry)
      data_head <- rep(data_head,ncount_entry)
      idx = i + maxId
      data_interId <- rep(idx,ncount_entry)
      res <-data.frame(FInterId=data_interId,FParentItemNo=data_head,FEntryId=1:ncount_entry,FSubItemNo=data_body,stringsAsFactors = F)
      return(res)

    }


  })
  #针对所有行进行合并

  mydata2 <- do.call('rbind',mydata)
  #返回结果

 return(mydata2)

}


#' 获取BOM拆分的最大号
#'
#' @param conn  连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bom_split_getMaxId()
bom_split_getMaxId <- function(conn=tsda::conn_rds('lcrds')) {

  sql <- paste0("select isnull(max(FInterId),0)  as FInterID from t_lcrds_bomSplit")
  res <- tsda::sql_select(conn=conn,sql_str = sql)
  info <- res$FInterID
  return(info)

}


#' BOM拆分删除数据
#'
#' @param conn 连接
#' @param data  数据
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' bom_split_upload_input()
bom_split_upload <- function(conn=tsda::conn_rds('lcrds'),
                                   data=bom_split())
                             {
  max_id = bom_split_getMaxId(conn = conn)
  data$FInterId = data$FInterId + max_id

  tsda::db_writeTable(conn = conn,table_name = 't_lcrds_bomSplit_input',r_object = data,append = TRUE)
  #删除多余数据
  sql_del <- paste0("delete from t_lcrds_bomSplit_input
where FParentItemNo in (
select  FParentItemNo from t_lcrds_bomSplit)")
  tsda::sql_update(conn=conn,sql_str = sql_del)
  #插入新增的数据
  sql_ins <- paste0("insert into t_lcrds_bomSplit
select * from t_lcrds_bomSplit_input")
  tsda::sql_update(conn = conn,sql_str = sql_ins)
  #清空临时表数据
  sql_truncate <- paste0("truncate table  t_lcrds_bomSplit_input")
  tsda::sql_update(conn=conn,sql_str = sql_truncate)


}

#' bom数据查询
#'
#' @param conn 连接
#' @param FBillNo 单据编号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bom_split_query()
bom_split_query <- function(conn=tsda::conn_rds('lcrds'),FBillNo='') {
  if (FBillNo ==''){
    sql <- paste0("select
 FInterId as 序号,
FParentItemNo as 主图号,
FEntryId  as 分录号,
FSubItemNo as 子图号
from t_lcrds_bomSplit
order by FInterId,FEntryId")
  }else{
    sql <- paste0("select
 FInterId as 序号,
FParentItemNo as 主图号,
FEntryId  as 分录号,
FSubItemNo as 子图号
from t_lcrds_bomSplit
where FParentItemNo ='",FBillNo,"'
order by FInterId,FEntryId")
  }

  res <- tsda::sql_select(conn = conn,sql_str = sql)
  ncount <- nrow(res)
  if(ncount >0){
    info <- res
  }else{
    info <- data.frame(`反馈结果`='未查到记录!',stringsAsFactors = F)
  }
  return(info)



}
