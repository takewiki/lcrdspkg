#' 获取件号的类型
#'
#' @param conn 连接
#' @param FchartNo 图号
#' @param keyNo 件号
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bom_getKeyNoType()
bom_getKeyNoType <- function(conn=tsda::conn_rds('lcrds'),FchartNo='YX200A714',keyNo=''){
  #定义局变变量，找到后不再继续查找
  find <- 0
  vars <-Ltab_get_uniqueVars(conn = conn,FchartNo = FchartNo)
  seq_LETTERS <- LETTERS[!LETTERS %in% c('G','L')]
  if(keyNo %in% vars & find ==0){
    type='var'
    find = 1
  }
  if(keyNo == '' & find == 0){
    type ='seq'
    find = 1
  }
  if(tsdo::left(keyNo) =='G' & find == 0){
    type='G'
    find =1
  }
  if(is.na(keyNo) & find == 0){
    type='NA'
    find = 1
  }
  if(tsdo::left(keyNo) %in% seq_LETTERS & find == 0){
    type='seq'
    find =1
  }
  if(tsdo::left(keyNo) =='-' & find == 0){
    type='seq'
    find =1
  }
  if(is.numeric(as.numeric(keyNo)) & find == 0){


    if(as.numeric(keyNo) >=0){
      type='fixed'
      find =1
    }else{
      type='seq'
      find =1
    }
  }


  return(type)


}




#' 针对从L表中的取出来的数值进行判断
#'
#' @param value 数值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' bom_getVarValueType()
bom_getVarValueType <- function(value){
  find =0
  value = as.character(value)
  #print(value)
  seq_LETTERS <- LETTERS[!LETTERS %in% c('G','L')]
  if(find == 0 &is.na(value) ){
    type='NA'
    find = 1
  }
  if(find == 0 & tsdo::left(value) =='G'){
    type ='G'
    find =1
  }
  if(find == 0 & tsdo::left(value) =='-' ){
    type='seq'
    find =1
  }
  if(find == 0 & tsdo::left(value) %in% seq_LETTERS ){
    type='seq'
    find =1
  }
  if(find == 0 & tsdo::len(value) == 0 ){
    type ='blank'
    find = 1
  }

  if(find == 0 &is.numeric(as.numeric(value)) ){


      type='length'
      find =1

  }

  return(type)
}


#' 针对字符串进行处理
#'
#' @param x 字符串
#' @param sep 分离符
#'
#' @return 返回值
#' @export
#'
#' @examples
#' sql_Ltab()
sql_Ltab <- function(x,sep=',') {
  x = tsdo::na_replace(x,'')
  if(x == ''){
    res = x
  }else{
    r_list =strsplit(x,sep)
    #针对数据进行打碎
    data = r_list[[1]]
    res = paste("'",data,"'",sep="",collapse = ',')

  }
  return(res)

}
